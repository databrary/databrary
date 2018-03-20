{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Ingest
  ( viewIngest
  , postIngest
  , detectParticipantCSV
  , runParticipantUpload
  -- for tests
  , mappingParser
  ) where

import Control.Arrow (right)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HMP
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Vector (Vector)
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (FileInfo(..))
import System.Posix.FilePath (takeExtension)

import Data.Csv.Contrib (parseCsvWithHeader, getHeaders, extractColumnsDistinctSample)
import qualified Databrary.JSON as JSON
import Databrary.Ops
import Databrary.Has
import Databrary.Model.Category
import Databrary.Model.Id
import Databrary.Model.Metric (Metric)
import Databrary.Model.Ingest
import Databrary.Model.Permission
import Databrary.Model.Measure
import Databrary.Model.Metric
import Databrary.Model.Record
import Databrary.Model.Volume
import Databrary.Model.VolumeMetric
import Databrary.Model.Container
import Databrary.Ingest.Action
import Databrary.Ingest.JSON
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.View.Form (FormHtml)
import Databrary.View.Ingest

viewIngest :: ActionRoute (Id Volume)
viewIngest = action GET (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  peeks $ blankForm . htmlIngestForm v s

postIngest :: ActionRoute (Id Volume)
postIngest = multipartAction $ action POST (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  a <- runFormFiles [("json", 16*1024*1024)] (Just $ htmlIngestForm v s) $ do
    csrfForm
    abort <- "abort" .:> deform
    abort ?!$> (,,)
      <$> ("run" .:> deform)
      <*> ("overwrite" .:> deform)
      <*> ("json" .:> do
              (fileInfo :: FileInfo JSON.Value) <- deform
              (deformCheck
                   "Must be JSON."
                   (\f -> fileContentType f `elem` ["text/json", "application/json"] || takeExtension (fileName f) == ".json")
                   fileInfo))
  r <- maybe
    (True <$ focusIO abortIngest)
    (\(r,o,j) -> runIngest $ right (map (unId . containerId . containerRow)) <$> ingestJSON v (fileContent j) r o)
    a
  unless r $ result $ response badRequest400 [] ("failed" :: String)
  peeks $ otherRouteResponse [] viewIngest (volumeId $ volumeRow v)

maxWidelyAcceptableHttpBodyFileSize :: Word64
maxWidelyAcceptableHttpBodyFileSize = 16*1024*1024

-- TODO: maybe put csv file save/retrieve in Databrary.Store module
detectParticipantCSV :: ActionRoute (Id Volume)
detectParticipantCSV = action POST (pathJSON >/> pathId </< "detectParticipantCSV") $ \vi -> withAuth $ do
    v <- getVolume PermissionEDIT vi
    csvFileInfo <-
      -- TODO: is Nothing okay here?
      runFormFiles [("file", maxWidelyAcceptableHttpBodyFileSize)] (Nothing :: Maybe (RequestContext -> FormHtml TL.Text)) $ do
          csrfForm
          fileInfo :: (FileInfo TL.Text) <- "file" .:> deform
          return fileInfo
    liftIO (print ("after extract form"))
    let uploadFileContents' = (BSL.toStrict . TLE.encodeUtf8 . fileContent) csvFileInfo
    liftIO (print "uploaded contents below")
    liftIO (print uploadFileContents')
    case parseCsvWithHeader uploadFileContents' of
        Left err -> do
            liftIO (print ("csv parse error", err))
            pure (response badRequest400 [] err)
        Right (hdrs, records) -> do
            participantMetrics <- lookupVolumeParticipantMetrics v
            liftIO (print ("before check determine", show hdrs))
            case checkDetermineMapping participantMetrics ((fmap TE.decodeUtf8 . getHeaders) hdrs) uploadFileContents' of
                Left err -> do
                    liftIO (print ("failed to determine mapping", err))
                    -- if column check failed, then don't save csv file and response is error
                    pure (response badRequest400 [] err)
                Right participantFieldMapping -> do
                    let uploadFileName = (BSC.unpack . fileName) csvFileInfo  -- TODO: add prefix to filename
                    liftIO (BS.writeFile ("/tmp/" ++ uploadFileName) uploadFileContents')
                    pure
                        $ okResponse []
                            $ JSON.recordEncoding -- TODO: not record encoding
                                $ JSON.Record vi
                                    $      "csv_upload_id" JSON..= uploadFileName
                                        <> "column_samples" JSON..= extractColumnsDistinctSampleJson 5 hdrs records
                                        <> "suggested_mapping" JSON..= participantFieldMappingToJSON participantFieldMapping
                                        <> "columns_firstvals" JSON..= extractColumnsInitialJson 5 hdrs records

runParticipantUpload :: ActionRoute (Id Volume)
runParticipantUpload = action POST (pathJSON >/> pathId </< "runParticipantUpload") $ \vi -> withAuth $ do
    v <- getVolume PermissionEDIT vi
    -- reqCtxt <- peek
    (csvUploadId :: String, selectedMapping :: JSON.Value) <- runForm (Nothing) $ do
        csrfForm
        (uploadId :: String) <- "csv_upload_id" .:> deform
        mapping <- "selected_mapping" .:> deform
        pure (uploadId, mapping)
    -- TODO: resolve csv id to absolute path; http error if unknown
    uploadFileContents <- (liftIO . BS.readFile) ("/tmp/" ++ csvUploadId)
    case JSON.parseEither mappingParser selectedMapping of
        Left err ->
            pure (response badRequest400 [] err) -- bad json shape or keys
        Right mpngVal -> do
            participantActiveMetrics <- lookupVolumeParticipantMetrics v
            case parseParticipantFieldMapping participantActiveMetrics mpngVal of
                Left err ->
                    pure (response badRequest400 [] err) -- mapping of inactive metrics or missing metric
                Right mpngs -> do
                    liftIO $ print ("upload id", csvUploadId, "mapping", mpngs)
                    case attemptParseRows mpngs uploadFileContents of
                        Left err ->   -- invalid value in row
                            pure (response badRequest400 [] err)
                        Right (hdrs, records) -> do
                            eRes <- runImport participantActiveMetrics v records
                            pure
                                $ okResponse []
                                    $ JSON.recordEncoding -- TODO: not record encoding
                                        $ JSON.Record vi $ "succeeded" JSON..= True

mappingParser :: JSON.Value -> JSON.Parser (Map Metric Text)
mappingParser val = do
    (entries :: [HeaderMappingEntry]) <- JSON.parseJSON val
    pure ((Map.fromList . fmap (\e -> (hmeMetric e, hmeCsvField e))) entries)

 -- TODO: error or count
runImport :: [Metric] -> Volume -> Vector ParticipantRecord -> ActionM (Vector ())
runImport activeMetrics vol records =
    mapM (createOrUpdateRecord activeMetrics vol) records

data ParticipantStatus = Create | Found Record
    -- deriving (Show, Eq)

data MeasureUpdateAction = Upsert Metric MeasureDatum | Unchanged
    deriving (Show, Eq)

data ParticipantRecordAction = ParticipantRecordAction ParticipantStatus [MeasureUpdateAction]
    -- deriving (Show, Eq)

buildParticipantRecordAction
    :: [Metric] -> Volume -> ParticipantRecord -> ParticipantStatus -> ParticipantRecordAction
buildParticipantRecordAction participantActiveMetrics vol participantRecord updatingRecord =
    let
        mId = getFieldVal' prdId "id"
        mInfo = getFieldVal' prdInfo "info"
        mDescription = getFieldVal' prdDescription "description"
        mBirthdate = getFieldVal' prdBirthdate "birthdate"
        mGender = getFieldVal' prdGender "gender"
        mRace = getFieldVal' prdRace "race"
        mEthnicity = getFieldVal' prdEthnicity "ethnicity"
        mGestationalAge = getFieldVal' prdGestationalAge "gestationalage"
        mPregnancyTerm = getFieldVal' prdPregnancyTerm "pregnancyterm"
        mBirthWeight = getFieldVal' prdBirthWeight "birthweight"
        mDisability = getFieldVal' prdDisability "disability"
        mLanguage = getFieldVal' prdLanguage "language"
        mCountry = getFieldVal' prdCountry "country"
        mState = getFieldVal' prdState "state"
        mSetting = getFieldVal' prdSetting "setting"
        -- print ("save measure id:", mId)
        measureActions =
            [ changeRecordMeasureIfUsed mId
            , changeRecordMeasureIfUsed mInfo
            , changeRecordMeasureIfUsed mDescription
            , changeRecordMeasureIfUsed mBirthdate
            , changeRecordMeasureIfUsed mGender
            , changeRecordMeasureIfUsed mRace
            , changeRecordMeasureIfUsed mEthnicity
            , changeRecordMeasureIfUsed mGestationalAge
            , changeRecordMeasureIfUsed mPregnancyTerm
            , changeRecordMeasureIfUsed mBirthWeight
            , changeRecordMeasureIfUsed mDisability
            , changeRecordMeasureIfUsed mLanguage
            , changeRecordMeasureIfUsed mCountry
            , changeRecordMeasureIfUsed mState
            , changeRecordMeasureIfUsed mSetting
            ]
    in
        ParticipantRecordAction updatingRecord (catMaybes measureActions)
  where
    changeRecordMeasureIfUsed :: Maybe (BS.ByteString, Metric) -> Maybe MeasureUpdateAction
    changeRecordMeasureIfUsed mValueMetric = do
        (val, met) <- mValueMetric
        pure (determineUpdatedMeasure val met)
    determineUpdatedMeasure :: BS.ByteString -> Metric -> MeasureUpdateAction
    determineUpdatedMeasure val met =
        case updatingRecord of
            Create ->
                Upsert met val
            Found record -> do
                 -- TODO: 
                 -- mOldVal <- getOldVal metric record
                 -- action = maybe (Upsert val) (\o -> if o == val then Unchanged else Upsert val)
                 let measureAction = Upsert met val
                 measureAction
    getFieldVal' :: (ParticipantRecord -> Maybe BS.ByteString) -> Text -> Maybe (BS.ByteString, Metric)
    getFieldVal' = getFieldVal participantActiveMetrics participantRecord

getFieldVal
    :: [Metric]
    -> ParticipantRecord 
    -> (ParticipantRecord -> Maybe BS.ByteString)
    -> Text
    -> Maybe (BS.ByteString, Metric)
getFieldVal participantActiveMetrics participantRecord extractFieldVal metricSymbolicName =
    case extractFieldVal participantRecord of
        Just fieldVal ->
            pure (fieldVal, findMetricBySymbolicName metricSymbolicName) -- <<<<<<<< lookup metric
        Nothing ->
            Nothing -- field isn't used by this volume, so don't need to save the measure
  where
    findMetricBySymbolicName :: Text -> Metric  -- TODO: copied from above, move to shared function
    findMetricBySymbolicName symbolicName =
        (  fromJust
         . L.find (\m -> (T.filter (/= ' ') . T.toLower . metricName) m == symbolicName))
        participantActiveMetrics
        
createOrUpdateRecord :: [Metric] -> Volume -> ParticipantRecord -> ActionM () -- TODO: error or record
createOrUpdateRecord participantActiveMetrics vol participantRecord = do
    let participantCategory = getCategory' (Id 1) -- TODO: use global variable
        (idVal, idMetric) = maybe (error "id missing") id (getFieldVal' prdId "id")
    mOldParticipant <- lookupVolumeParticipant vol idVal
    let recordStatus =
            case mOldParticipant of
                Nothing -> Create
                Just oldParticipant -> Found oldParticipant
    -- print ("save measure id:", mId)
    case buildParticipantRecordAction participantActiveMetrics vol participantRecord recordStatus of
        ParticipantRecordAction Create measureActs -> do
            newParticipantShell <- addRecord (blankRecord participantCategory vol) -- blankParticipantRecord
            _ <- mapM (runMeasureUpdate newParticipantShell) measureActs
            pure () -- TODO: reload participant
        ParticipantRecordAction (Found oldRecord) measureActs -> do
            _ <- mapM (runMeasureUpdate oldRecord) measureActs
            pure ()
  where
    runMeasureUpdate :: Record -> MeasureUpdateAction -> ActionM (Maybe Record)
    runMeasureUpdate record act =
        case act of
            Upsert met val -> changeRecordMeasure (Measure record met val)
            Unchanged -> pure Nothing
    getFieldVal' :: (ParticipantRecord -> Maybe BS.ByteString) -> Text -> Maybe (BS.ByteString, Metric)
    getFieldVal' = getFieldVal participantActiveMetrics participantRecord
