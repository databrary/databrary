{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- How to run Databrary Actions.
--
-- This module is the foundation of the site. It provides a method for packaging
-- up route actions into 'Wai.Application's (which is effectively the HTTP
-- transport layer), as well as a method for packaging them into background
-- threads.
module Action.Run
  (
  -- * \"Framework\" code
    actionApp
  , forkAction
  -- * Declaring authentication needs for Actions
  , withAuth
  , withoutAuth
  , withReAuth
  -- * The underlying type
  , Action
  ) where

import Control.Concurrent (ThreadId, forkFinally)
import Control.Exception (SomeException)
import Control.Monad.Reader (ReaderT(..), withReaderT, local)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate, hCacheControl, methodHead)
import qualified Network.Wai as Wai

import Has
import HTTP
import Service.DB
import Service.Types
import Service.Log
import Model.Identity
import Model.Id.Types
import Model.Party.Types
import HTTP.Request
import Context
import Action.Types
import Action.Request
import Action.Response

-- |
-- Transform a web request to a lower-level action.
--
-- Given an action that runs in a top-level Handler, build the necessary context
-- for that action, and run in it in the base-level ActionContextM
--
-- Handler has a richer context: it has all of ActionContext, plus Identity and
-- the Wai Request.
withHandler
    :: forall a
     . Request -- ^ The wai request to handle
    -> Identity -- ^ The identity to use for this action
    -> Handler a -- ^ The action to perform
    -> ActionContextM a -- ^ The base-level control access to the system
withHandler waiReq identity h =
    let (handler :: ReaderT RequestContext IO a) = unHandler h
    in withReaderT (\(c :: ActionContext) -> RequestContext c waiReq identity) handler

-- | Authentication requirements for an 'Action'.
data NeedsAuthentication = NeedsAuthentication | DoesntNeedAuthentication

-- | This type captures both the authentication needs and the handler for a
-- route.
--
-- It extends ActionRoute, which already has most info about a route and how to
-- serve it, to include the authentication requirement.
data Action = Action
    { _actionAuthentication :: !NeedsAuthentication
    , _actionM :: !(Handler Response)
    }

-- | Convert an Action into a Wai.Application.
--
-- This is the 3rd level of the Databrary \"web framework\". It runs the
-- requested action with some resolved identity to build the HTTP response. See
-- 'actionRouteApp' for level 2, and 'servantApp' for level 1.
--
-- TODO: For converting to Servant, this whole function should be duplicated by
-- new combinators.
--
-- For instance, there is a section (in the second 'let', within the do) where
-- headers are added to the response. Servant requires us to put that in the
-- type, which can be easily done. We can even make a combinator for looking up
-- auth results and logging access. After all that, we should be able to create
-- something with HoistServer that will run the rest of the (Databrary) Handler.
--
-- But then we'll have to map the Response (Action ~ ReaderT RequestContext IO
-- Response) into something else! And how do we catch exceptions? Plain old
-- catch blocks? And what do we convert exceptions into?
actionApp
    :: Service -- ^ All the low-level system capabilities
    -> Action -- ^ Action to run
    -> Wai.Application -- ^ Callback for Wai
actionApp service (Action needsAuth act) waiReq waiSend =
    let
        isdb = isDatabraryClient waiReq
        authenticatedAct :: ActionContextM (Identity, Response)
        authenticatedAct = do
            sec <- peek
            conn <- peek
            identity <- fetchIdent sec conn waiReq needsAuth
            waiResponse <-
                ReaderT
                    -- runResult unwraps the short-circuit machinery from
                    -- "Action.Response", returning IO Response.
                    $ \actCtx ->
                        runResult
                            (runHandler act (RequestContext actCtx waiReq identity))
            return (identity, waiResponse)
      in do
          ts <- getCurrentTime
          (identityUsed, waiResponse) <- runContextM authenticatedAct service
          logAccess
              ts
              waiReq
              (extractFromIdentifiedSessOrDefault
                  Nothing
                  (Just . (show :: Id Party -> String) . view)
                  identityUsed)
              waiResponse
              (serviceLogs service)
          let
              waiResponse' = Wai.mapResponseHeaders
                  (((hDate, formatHTTPTimestamp ts) :)
                    . (if isdb then ((hCacheControl, "no-cache") :) else id))
                  waiResponse
          waiSend $ if Wai.requestMethod waiReq == methodHead
              then emptyResponse
                  (Wai.responseStatus waiResponse')
                  (Wai.responseHeaders waiResponse')
              else waiResponse'

-- | Special-purpose context for determining the user's identity. We don't need
-- the full Handler for that, and since this is sensitive work, we don't want to
-- just cheat and use it anyway.
data IdContext = IdContext
    { ctxReq :: Wai.Request
    , ctxSec :: Secret
    , ctxConn :: DBConn
    }

instance Has Wai.Request IdContext where view = ctxReq
instance Has Secret IdContext where view = ctxSec
instance Has DBConn IdContext where view = ctxConn

-- | Look up the user's identity (or don't)
fetchIdent
    :: Secret -- ^ Session key
    -> DBConn -- ^ For querying the session table
    -> Wai.Request
    -- ^ FIXME: Why the entire request? Can we narrow the scope? What is
    -- actually needed is the session cookie.
    -> NeedsAuthentication
    -- ^ Whether or not to actually do the lookup.
    --
    -- FIXME: This seems like an unnecessary complication.
    -> ActionContextM Identity
fetchIdent sec con waiReq = \case
    NeedsAuthentication ->
        runReaderT determineIdentity (IdContext waiReq sec con)
    DoesntNeedAuthentication -> return IdentityNotNeeded

-- | Run a Handler action in the background (IO).
--
-- A new ActionContextM is built, with a fresh guaranteed db connection and
-- timestamp, via 'runContextM'
forkAction
    :: Handler a -- ^ Handler action to run
    -> RequestContext
    -- ^ Original context
    --
    -- The background action inherits this context's 'Request' and 'Service'.
    -> (Either SomeException a -> IO ())
    -- ^ Cleanup to run when the action exits
    -> IO ThreadId
forkAction h reqCtx = forkFinally $ runContextM (withHandler req ident h) srv
  where
    req = contextRequest reqCtx
    ident = requestIdentity reqCtx
    srv = contextService (requestContext reqCtx)

-- | Tag an 'Action' as needing to know whether or not the user is
-- authenticated. This simply triggers an extra session lookup in 'actionApp'.
withAuth
    :: Handler Response -- ^ The handler for the route
    -> Action -- ^ The bundled action
withAuth = Action NeedsAuthentication

-- | Tag an 'Action' as not needing auth. See note at 'withAuth'.
withoutAuth :: Handler Response -> Action
withoutAuth = Action DoesntNeedAuthentication

-- | This may be like a 'su' that allows running an action as a different
-- identity.
withReAuth
    :: SiteAuth -- ^ The identity to assume
    -> Handler a
    -- ^ The action to perform as the assumed identity
    --
    -- Note that one might argue for an indexed form of 'Handler' here, a la
    -- Selda and subqueries.
    -> Handler a -- ^ The re-authenticated action, packaged into the original context
withReAuth u =
    Handler
        . local (\a -> a { requestIdentity = ReIdentified u })
        . unHandler
