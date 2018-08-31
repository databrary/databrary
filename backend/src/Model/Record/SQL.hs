{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Model.Record.SQL
  ( selectVolumeRecord
  , selectRecord
  , insertRecord
  , updateRecord
  , deleteRecord
  , makeRecord
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Model.SQL.Select
import Model.Audit.SQL
import Model.Id.Types
import Model.Release.Types
import Model.Volume.Types
import Model.Volume.SQL
import Model.Category
import Model.Metric
import Model.Record.Types

import qualified Text.Read.Located

parseMeasure :: Record -> BSC.ByteString -> Measure
parseMeasure r s = Measure r (getMetric' (Id (Text.Read.Located.read (BSC.unpack m)))) (BSC.tail d) where
  (m, d) = BSC.splitAt (fromMaybe (error $ "parseMeasure " ++ show (recordId $ recordRow r) ++ ": " ++ BSC.unpack s) $ BSC.elemIndex ':' s) s

makeRecord :: Id Record -> Id Category -> [Maybe BSC.ByteString] -> Maybe Release -> Volume -> Record
makeRecord i c ms p v = r where
  r = Record (RecordRow i (getCategory' c)) (map (parseMeasure r . fromMaybe (error "NULL record.measure")) ms) p v

selectRecordRow :: Selector -- ^ @Maybe 'Release' -> 'Volume' -> 'Record'@
selectRecordRow = fromMap ("record_measures AS " ++) $
  selectColumns 'makeRecord "record" ["id", "category", "measures"]

selectVolumeRecord :: Selector -- ^ @'Volume' -> 'Record'@
selectVolumeRecord = addSelects '($) selectRecordRow [SelectExpr "record_release(record.id)"] -- XXX explicit table reference (throughout)

selectRecord :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Record'@
selectRecord ident = selectJoin '($)
  [ selectVolumeRecord
  , joinOn "record.volume = volume.id" $ selectVolume ident
  ]

recordKeys :: String -- ^ @'Record'@
  -> [(String, String)]
recordKeys r =
  [ ("id", "${recordId $ recordRow " ++ r ++ "}") ]

recordSets :: String -- ^ @'Record'@
  -> [(String, String)]
recordSets r =
  [ ("volume", "${volumeId $ volumeRow $ recordVolume " ++ r ++ "}")
  , ("category", "${categoryId $ recordCategory $ recordRow " ++ r ++ "}")
  ]

setRecordId :: Record -> Id Record -> Record
setRecordId r i = r{ recordRow = (recordRow r){ recordId = i } }

insertRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Record'@
  -> TH.ExpQ -- ^ @'Record'@
insertRecord ident r = auditInsert ident "record"
  (recordSets (nameRef r))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setRecordId `TH.AppE` TH.VarE r) `TH.AppE`) $ selectColumn "record" "id")

updateRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Record'@
  -> TH.ExpQ -- ^ @()@
updateRecord ident r = auditUpdate ident "record"
  (recordSets (nameRef r))
  (whereEq $ recordKeys (nameRef r))
  Nothing

deleteRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Record'@
  -> TH.ExpQ -- ^ @()@
deleteRecord ident r = auditDelete ident "record"
  (whereEq $ recordKeys (nameRef r))
  Nothing
