{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Authorize.Types
  ( Authorization(..)
  , Authorize(..)
  , mkAuthorizeRequest
  , with1210Utc
  ) where

import Data.Time

import Databrary.Has (Has(..))
import Databrary.Model.Time
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types

data Authorization = Authorization
  { authorizeAccess :: !Access
  , authorizeChild :: Party
  , authorizeParent :: Party
  }

-- makeHasRec ''Authorization ['authorizeAccess]
instance Has Access Authorization where
  view = authorizeAccess

data Authorize = Authorize
  { authorization :: Authorization
  , authorizeExpires :: Maybe Timestamp
  }

-- makeHasRec ''Authorize ['authorization]
-- instance Has Authorization Authorize where
--   view = authorization
instance Has Access Authorize where
  view = (view . authorization)

mkAuthorizeRequest :: Party -> Party -> Authorize
mkAuthorizeRequest child parent =
    Authorize (Authorization mempty child parent) Nothing

with1210Utc :: Day -> UTCTime
with1210Utc d = UTCTime d 43210
