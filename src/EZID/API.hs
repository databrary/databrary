{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module EZID.API
  ( EZIDM
  , runEZIDM
  , ezidStatus
  , EZIDMeta(..)
  , ezidCreate
  , ezidModify
  ) where

import Control.Arrow (left)
import Control.Exception.Lifted (try)
import Control.Monad ((<=<), join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (methodGet, methodPut, methodPost)
import Network.HTTP.Types.Status (statusCode)
import Network.URI (URI)
import qualified Text.XML.Light as XML

import Ops
import Has
import Model.Id.Types
import Model.Identity.Types
import Model.Party.Types
import Model.Permission.Types
import Service.DB
import Service.Types
import Service.Log
import Context
import HTTP.Client
import EZID.Service
import qualified EZID.ANVL as ANVL
import EZID.DataCite

data EZIDContext = EZIDContext
  { ezidContext :: !BackgroundContext
  , contextEZID :: !EZID
  }

instance Has Model.Permission.Types.Access EZIDContext where
  view = view . ezidContext
instance Has (Model.Id.Types.Id Model.Party.Types.Party) EZIDContext where
  view = view . ezidContext
instance Has Model.Party.Types.Party EZIDContext where
   view = view . ezidContext
instance Has Model.Party.Types.SiteAuth EZIDContext where
  view = view . ezidContext
instance Has Model.Identity.Types.Identity EZIDContext where
  view = view . ezidContext
instance Has Service.DB.DBConn EZIDContext where
  view = view . ezidContext
instance Has Logs EZIDContext where
  view = view . ezidContext
instance Has HTTPClient EZIDContext where
  view = view . ezidContext
instance Has EZID EZIDContext where
  view = contextEZID

type EZIDM a = CookiesT (ReaderT EZIDContext IO) a

runEZIDM :: EZIDM a -> BackgroundContextM (Maybe a)
runEZIDM f = ReaderT $ \ctx ->
  mapM (runReaderT (runCookiesT f) . EZIDContext ctx)
    (serviceEZID $ contextService $ backgroundContext ctx)

ezidCall :: BS.ByteString -> BS.ByteString -> ANVL.ANVL -> EZIDM (Maybe ANVL.ANVL)
ezidCall path method body = do
  req <- peeks ezidRequest
  t <- liftIO getCurrentTime
  r <- try $ withResponseCookies (requestAcceptContent "text/plain" req)
    { HC.path = path
    , HC.method = method
    , HC.requestBody = HC.RequestBodyLBS $ B.toLazyByteString $ ANVL.encode body
    } (fmap P.eitherResult . httpParse ANVL.parse)
  let r' = join $ left (show :: HC.HttpException -> String) r
  focusIO $ logMsg t $ toLogStr ("ezid: " <> method <> " " <> path <> ": ") <> toLogStr (either id show r')
  return $ rightJust r'

ezidCheck :: ANVL.ANVL -> Maybe T.Text
ezidCheck = lookup "success"

ezidStatus :: EZIDM Bool
ezidStatus =
  isJust . (ezidCheck =<<) <$> ezidCall "/heartbeat" methodGet []

data EZIDMeta
  = EZIDPublic
    { ezidTarget :: !URI
    , ezidDataCite :: !DataCite
    }
  | EZIDUnavailable

ezidMeta :: EZIDMeta -> ANVL.ANVL
ezidMeta EZIDPublic{..} =
  [ ("_target", T.pack $ show ezidTarget)
  , ("_status", "public")
  , ("_profile", "datacite")
  , ("datacite", T.pack $ XML.showTopElement $ dataCiteXML ezidDataCite)
  ]
ezidMeta EZIDUnavailable = [ ("_status", "unavailable") ]

ezidCreate :: BS.ByteString -> EZIDMeta -> EZIDM (Maybe BS.ByteString)
ezidCreate hdl meta = do
  ns <- peeks ezidNS
  fmap (TE.encodeUtf8 . T.takeWhile (\c -> c /= '|' && not (isSpace c))) . (=<<) (T.stripPrefix "doi:" <=< ezidCheck) <$>
    ezidCall "/dois" methodPost (ezidMeta meta)

ezidModify :: BS.ByteString -> EZIDMeta -> EZIDM Bool
ezidModify hdl meta =
  isJust . (ezidCheck =<<) <$>
      ezidCall ("/dois/" <> hdl) methodPut (ezidMeta meta)
