{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, OverloadedStrings #-}
module Action.Response
  ( Response
  , ResponseData(..)
  , emptyResponse
  , okResponse
  , result
  , unsafeResult
  , runResult
  , proxyResponse
  ) where

import Conduit (Source, (.|))
import Control.Exception (Exception, throwIO, throw, handle)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable)
import Network.HTTP.Types (ResponseHeaders, Status, ok200, hContentType)
import Network.Wai
    ( Response
    , responseBuilder
    , responseLBS
    , StreamingBody
    , responseStream
    , FilePart(..)
    , responseFile
    , responseStatus
    )
import System.Posix.Types (FileOffset)
import qualified Conduit as CND
import qualified Data.Binary.Builder as DBB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client as HC
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html

import qualified JSON

-- | This class captures Databrary's mechanism for creating 'Response's from the
-- values actually returned by handlers.
--
-- It is rather general. A value can be turned into a response either by
-- modifying headers or by using one of Wai's Response composers (e.g.
-- 'responseBuilder'). There are some rather esoteric instances.
--
-- Similar mechanisms in Servant or Yesod generally restrict themselves to
-- building response *data* out of return values, without explicit mention of
-- 'Response'. See e.g.
-- <http://hackage.haskell.org/package/http-api-data-0.3.8.1/docs/Web-HttpApiData.html>
--
-- Servant also has
-- <http://hackage.haskell.org/package/servant-0.13.0.1/docs/Servant-API-ContentTypes.html#t:MimeRender MimeRender>,
-- which describes how to create a ByteString for use in creating Responses.
class ResponseData r where
  response :: Status -> ResponseHeaders -> r -> Response

instance ResponseData BSB.Builder where
  response = responseBuilder

instance ResponseData BSL.ByteString where
  response = responseLBS

instance ResponseData BS.ByteString where
  response s h = responseBuilder s h . BSB.byteString

instance ResponseData (Source (CND.ResourceT IO) BS.ByteString) where
    response s h src =
        responseStream s h
            (\send _ ->
                CND.runConduitRes
                    (src
                    .| CND.mapM_C
                        (CND.lift . send . DBB.fromByteString)))

instance ResponseData StreamingBody where
  response = responseStream

instance ResponseData ((BS.ByteString -> IO ()) -> IO ()) where
  response s h f = responseStream s h (\w l -> f (\b -> if BS.null b then l else w (BSB.byteString b)))

instance ResponseData (FilePath, Maybe FilePart) where
  response s h (f, p) = responseFile s h f p

instance ResponseData (FilePath, FilePart) where
  response s h (f, p) = response s h (f, Just p)

instance ResponseData (FilePath, Maybe FileOffset) where
  response s h (f, z) = response s h (f, join (FilePart 0) . toInteger <$> z)

instance ResponseData String where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . BSB.stringUtf8

instance ResponseData T.Text where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . TE.encodeUtf8Builder

instance ResponseData TL.Text where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . TLE.encodeUtf8Builder

instance ResponseData JSON.Value where
  response s h =
    response s ((hContentType, "application/json") : h) . JSON.encode

instance ResponseData JSON.Encoding where
  response s h =
    response s ((hContentType, "application/json") : h) . JSON.fromEncoding

instance ResponseData JSON.Series where
  response s h =
    response s h . JSON.pairs

instance (JSON.ToJSON k, JSON.ToObject o, ResponseData o) => ResponseData (JSON.Record k o) where
  response s h =
    response s h . JSON.recordObject

instance ResponseData Html.Html where
  response s h =
    response s ((hContentType, "text/html;charset=utf-8") : h) . Html.renderHtmlBuilder

emptyResponse :: Status -> ResponseHeaders -> Response
emptyResponse s h = response s h BS.empty

okResponse :: ResponseData r => ResponseHeaders -> r -> Response
okResponse = response ok200

-- | A wrapper for the short-circuiting machinery (see 'result').
newtype Result = Result { resultResponse :: Response } deriving (Typeable)
instance Show Result where
  showsPrec p (Result r) = showParen (p > 10)
    $ showString "Result " . showsPrec 11 (responseStatus r)
instance Exception Result

-- | Short circuit immediately, returning the given Response.
--
-- FIXME: Rather than implementing this using exceptions, could we either use a
-- left-biased Alternative or simply use a better procedural style?
result :: MonadIO m => Response -> m a
result = liftIO . throwIO . Result

-- | Short circuit from within non-monadic code. Not recommended, and hardly
-- ever used.
unsafeResult :: Response -> a
unsafeResult = throw . Result

-- | Run some action that may short circuit using 'result' or 'unsafeResult'.
runResult :: IO Response -> IO Response
runResult = handle (return . resultResponse)

proxyResponse :: HC.Response BSL.ByteString -> Response
proxyResponse r = responseLBS
  (HC.responseStatus r)
  (filter ((/= "transfer-encoding") . fst) $ HC.responseHeaders r)
  (HC.responseBody r)
