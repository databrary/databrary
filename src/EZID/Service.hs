{-# LANGUAGE OverloadedStrings #-}
module EZID.Service
  ( EZID(..)
  , initEZID
  ) where

import Control.Monad (unless, forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (hContentType)

import qualified Store.Config as C

data EZID = EZID
  { ezidRequest :: !HC.Request
  , ezidNS :: !BS.ByteString
  }

initEZID :: C.Config -> IO (Maybe EZID)
initEZID conf = conf C.! "ns" `forM` \ns -> do
  unless ("doi:10." `BSC.isPrefixOf` ns) $
    fail "ezid.ns must be for DOIs"
  req <- HC.parseRequest "http://ez.datacite.org"
  return $ EZID
    { ezidRequest = HC.applyBasicAuth (conf C.! "user") (conf C.! "pass") req
      { HC.requestHeaders = (hContentType, "text/plain") : HC.requestHeaders req
      , HC.responseTimeout = HC.responseTimeoutMicro 100000000
      , HC.redirectCount = 1
      }
    , ezidNS = ns
    }
