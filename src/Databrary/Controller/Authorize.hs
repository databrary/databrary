{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Authorize
  ( viewAuthorize
  , postAuthorize
  , deleteAuthorize
  , postAuthorizeNotFound
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, liftM3, mfilter, forM_)
import Data.Function (on)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime(..), fromGregorian, addGregorianYearsRollOver)
import Network.HTTP.Types (noContent204)

import Databrary.Ops
import Databrary.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB (MonadDB)
import Databrary.Service.Mail
import Databrary.Static.Service
import Databrary.Model.Audit (MonadAudit)
-- import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Notification.Types
import Databrary.Model.Authorize
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Party
import Databrary.Controller.Notification
import Databrary.View.Authorize

-- Every route in this module asserts the requesting user has ADMIN permissions over the primary (first) party
--  referenced in the request. The requesting user must have ADMIN permissions because they are viewing/editing
--  the primary party's authorization relationships. Typically the requesting user will be the primary party,
--  but the requesting user can also be an admin that the primary party has delegated control to.

viewAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
viewAuthorize = action GET (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  (_, c') <- findOrMakeDefault child parent
  case api of
    JSON -> return $ okResponse [] $ JSON.pairs $ authorizeJSON c'
    HTML
      | app -> return $ okResponse [] ("" :: T.Text) -- TODO
      -- If the request is viewing an authorize from parent to child, then present edit form
      | otherwise -> peeks $ blankForm . htmlAuthorizeForm c'

partyDelegates :: (MonadDB c m, MonadHasIdentity c m) => Party -> m [Account]
partyDelegates u = do
  l <- deleg u
  if null l
    then deleg rootParty
    else return l
  where
  deleg p = mapMaybe partyAccount . (p :)
    . map (authorizeChild . authorization)
    <$> lookupAuthorizedChildren p (Just PermissionADMIN)

removeAuthorizeNotify :: Maybe Authorize -> Handler ()
removeAuthorizeNotify priorAuth =
    let noReplacementAuthorization = Nothing
    in updateAuthorize priorAuth noReplacementAuthorization

-- | Remove (when only first argument provided) or create/swap in new version of authorization, triggering notifications.
-- Do nothing when neither an old or new auth has been provided.
updateAuthorize :: Maybe Authorize -> Maybe Authorize -> Handler ()
updateAuthorize priorAuth newOrUpdatedAuth
  | Just priorElseNewCore <- authorization <$> (priorAuth <|> newOrUpdatedAuth :: Maybe Authorize) = do
  maybe
    (mapM_ removeAuthorize priorAuth)
    changeAuthorize
    newOrUpdatedAuth
  when (on (/=) (foldMap $ authorizeAccess . authorization) newOrUpdatedAuth priorAuth) $ do
    let perm = accessSite <$> newOrUpdatedAuth
    dl <- partyDelegates $ authorizeParent priorElseNewCore
    forM_ dl $ \t ->
      createNotification (blankNotification t NoticeAuthorizeChildGranted)
        { notificationParty = Just $ partyRow $ authorizeChild priorElseNewCore
        , notificationPermission = perm
        }
    forM_ (partyAccount $ authorizeChild priorElseNewCore) $ \t ->
      createNotification (blankNotification t NoticeAuthorizeGranted)
        { notificationParty = Just $ partyRow $ authorizeParent priorElseNewCore
        , notificationPermission = perm
        }
  updateAuthorizeNotifications priorAuth
      $ fromMaybe (Authorize priorElseNewCore{ authorizeAccess = mempty } Nothing) newOrUpdatedAuth
updateAuthorize ~Nothing ~Nothing = return ()

createAuthorize :: (MonadAudit c m) => Authorize -> m ()
createAuthorize = changeAuthorize

-- | Either create a new authorization request from PartyTarget child to a parent or
-- update/create/reject with validation errors an authorization request to the PartyTarget parent from a child
postAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
postAuthorize = action POST (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \arg@(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction . mfilter isNobodyParty =<< lookupParty oi -- Don't allow applying to or authorization request from nobody
  let (child, parent) = if app then (p, o) else (o, p)
  (c, c') <- findOrMakeDefault child parent
  resultingAuthorize <- if app
    -- The request involves a child party applying for authorization from a parent party
    then do
      when (isNothing c) $ do -- if there is no pending request or existing authorization
        createAuthorize c'
        dl <- partyDelegates o
        forM_ dl $ \t ->
          createNotification (blankNotification t NoticeAuthorizeChildRequest)
            { notificationParty = Just $ partyRow o }
        forM_ (partyAccount p) $ \t ->
          createNotification (blankNotification t NoticeAuthorizeRequest)
            { notificationParty = Just $ partyRow o }
      -- make either the newly created request or the existing found request the result value
      -- of this block
      return $ Just c'
    -- The request involves a parent party either creating or acting upon an existing authorization request from a child party
    else do
      su <- peeks identityAdmin
      now <- peek
      let maxexp = addGregorianYearsRollOver 2 $ utctDay now -- TODO: use timestamp from actioncontext instead of now?
          minexp = fromGregorian 2000 1 1
      -- the new version of this authorization (possibly first) should either be ...
      a <- runForm ((api == HTML) `thenUse` (htmlAuthorizeForm c')) $ do
        csrfForm
        delete <- "delete" .:> deform
        -- 1. Nothing (causing deletion if there was a request or old auth)
        delete `unlessReturn` (do
          site <- "site" .:> deform
          member <- "member" .:> deform
          expires <-
            "expires" .:>
              (deformCheck "Expiration must be within two years." (all (\e -> su || e > minexp && e <= maxexp))
               =<< (<|> (su `unlessUse` maxexp)) <$> deformNonEmpty deform)
          -- 2. A Just with the new or updated approved authorization
          return $ makeAuthorize (Access site member) (fmap with1210Utc expires) child parent)
      -- Perform the indicated change decided above in the value of "a"
      updateAuthorize c a
      return a
  case api of
    -- respond with a copy of the updated authorization, if any
    JSON -> return $ okResponse [] $ JSON.pairs $ foldMap authorizeJSON resultingAuthorize <> "party" JSON..=: partyJSON o
    HTML -> peeks $ otherRouteResponse [] viewAuthorize arg

findOrMakeDefault :: (MonadDB c m) => Party -> Party -> m (Maybe Authorize, Authorize)
findOrMakeDefault child parent = do
  c <- lookupAuthorize child parent
  pure (c, unendingNoPrivilegeAuthorize child parent `fromMaybe` c)

-- | If present, delete either a prior request for authorization. The authorization to delete can be specified
-- from the child perspective (child party is pathPartyTarget) or the parent perspective (parent party is pathPartyTarget).
-- Inform all relevant parties that the authorization has been deleted.
deleteAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
deleteAuthorize = action DELETE (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \arg@(api, i, AuthorizeTarget apply oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  (o :: Party) <- do
      mAuthorizeTargetParty <- lookupParty oi
      maybeAction mAuthorizeTargetParty
  let (child, parent) = if apply then (p, o) else (o, p)
  mAuth <- lookupAuthorize child parent
  removeAuthorizeNotify mAuth
  case api of
    JSON -> return $ okResponse [] $ JSON.pairs $ "party" JSON..=: partyJSON o
    HTML -> peeks $ otherRouteResponse [] viewAuthorize arg

-- | During registration and when requesting additional sponsors, if the target parent party doesn't exist in
-- Databrary yet, this route enables a user to submit some information on which target parent (AI or institution)
-- they are seeking, to trigger an email to the Databrary site admins, with the hope that the site admins are able
-- to manually get the intended parties into Databrary.
postAuthorizeNotFound :: ActionRoute (PartyTarget)
postAuthorizeNotFound = action POST (pathJSON >/> pathPartyTarget </< "notfound") $ \i -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  agent <- peeks $ fmap accountEmail . partyAccount
  (name, perm, info) <- runForm Nothing $ liftM3 (,,)
    ("name" .:> deform)
    ("permission" .:> deform)
    ("info" .:> deformNonEmpty deform)
  authaddr <- peeks staticAuthorizeAddr
  title <- peeks $ authorizeSiteTitle perm
  sendMail [Left authaddr] []
    ("Databrary authorization request from " <> partyName (partyRow p))
    $ TL.fromChunks [partyName $ partyRow p, " <", foldMap TE.decodeLatin1 agent, ">", mbt $ partyAffiliation $ partyRow p, " has requested to be authorized as an ", title, " by ", name, mbt info, ".\n"]
  return $ emptyResponse noContent204 []
  where mbt = maybe "" $ \t -> " (" <> t <> ")"
