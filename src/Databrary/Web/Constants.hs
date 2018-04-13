{-# LANGUAGE OverloadedStrings, CPP #-}
module Databrary.Web.Constants
  ( constantsJSON
  , constantsJS
  , generateConstantsJSON
  , generateConstantsJS
  ) where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Version (showVersion)
import System.IO (withBinaryFile, IOMode(WriteMode))

import Paths_databrary (version)
import qualified Databrary.JSON as JSON
import Databrary.Model.Enum
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Metric
import Databrary.Model.Category
import Databrary.Model.Format
import Databrary.Model.Party
import Databrary.Model.Notification.Notice
import Databrary.Web.Types
import Databrary.Web.Generate

constantsJSON :: JSON.ToNestedObject o u => Bool -> o
constantsJSON notificationBar =
     "permission" JSON..= enumValues PermissionPUBLIC
  <> "release" JSON..= enumValues ReleasePUBLIC
  <> "metric" JSON..=. JSON.recordMap (map metricJSON allMetrics)
  <> "category" JSON..=. JSON.recordMap (map categoryJSON allCategories)
  <> "format" JSON..=. JSON.recordMap (map formatJSON allFormats)
  <> "party" JSON..=.
    (  "nobody" JSON..=: partyJSON nobodyParty
    <> "root" JSON..=: partyJSON rootParty
    <> "staff" JSON..=: partyJSON staffParty
    )
  <> "notice" JSON..= JSON.object [ T.pack (show n) JSON..= n | n <- [minBound..maxBound::Notice] ]
  <> "delivery" JSON..= enumValues DeliveryNone
  <> "version" JSON..= showVersion version
#ifdef DEVEL
  <> "devel" JSON..= True
#endif
  <> "notificationbar" JSON..= notificationBar
-- #ifdef SANDBOX
--   <> "sandbox" JSON..= True
-- #endif
  -- TODO: url?
  where
  enumValues :: forall a . DBEnum a => a -> [String]
  enumValues _ = map show $ enumFromTo minBound (maxBound :: a)

constantsJSONB :: Bool -> BSB.Builder
constantsJSONB notificationBar = JSON.fromEncoding $ JSON.objectEncoding (constantsJSON notificationBar)

constantsJS :: Bool -> BSB.Builder
constantsJS notificationBar =
    BSB.string8 "app.constant('constantData'," <> (constantsJSONB notificationBar) <> BSB.string8 ");"

regenerateConstants :: BSB.Builder -> WebGenerator
regenerateConstants b = staticWebGenerate $ \f ->
  withBinaryFile f WriteMode $ \h ->
    BSB.hPutBuilder h b

generateConstantsJSON :: Bool -> WebGenerator
generateConstantsJSON notificationBar = regenerateConstants (constantsJSONB notificationBar)

generateConstantsJS :: Bool -> WebGenerator
generateConstantsJS notificationBar = regenerateConstants (constantsJS notificationBar)
