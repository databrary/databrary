{-# LANGUAGE OverloadedStrings #-}
module Controller.Zip
  ( zipContainer
  , zipVolume
  , viewVolumeDescription
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.List (groupBy, partition)
import Data.Maybe (fromJust, maybeToList)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.RangeSet.List as RS
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hContentType, hCacheControl, hContentLength)
import System.Posix.FilePath ((<.>))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html
import qualified Codec.Archive.Zip as ZIP
import qualified System.IO as IO
import qualified System.Directory as DIR
import qualified Conduit as CND
import Path (parseRelFile)

import Ops
import Has (view, peek, peeks)
import Store.Asset
import Store.Filename
import Store.CSV (buildCSV)
import Store.Types
import Model.Id
import Model.Permission
import Model.Volume
import Model.Container
import Model.Slot
import Model.RecordSlot
import Model.Asset
import Model.AssetSlot
import Model.Format
import Model.Party
import Model.Citation
import Model.Funding
import HTTP
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Asset
import Controller.Container
import Controller.Volume
import Controller.Party
import Controller.CSV
import Controller.Angular
import Controller.IdSet
import View.Zip

-- isOrig flags have been added to toggle the ability to access the pre-transcoded asset
assetZipEntry2 :: Bool -> BS.ByteString -> AssetSlot -> Handler (ZIP.ZipArchive ())
assetZipEntry2 isOrig containerDir AssetSlot{ slotAsset = a@Asset{ assetRow = ar@AssetRow{ assetId = aid}}} = do
  origAsset <- lookupOrigAsset aid
  Just f <- case isOrig of
                 True -> getAssetFile $ fromJust origAsset
                 False -> getAssetFile a
  req <- peek
  let entryName =
        containerDir `BS.append` (case isOrig of
          False -> makeFilename (assetDownloadName True False ar) `addFormatExtension` assetFormat ar
          True -> makeFilename (assetDownloadName False True ar) `addFormatExtension` assetFormat ar)
  entrySelector <- liftIO (parseRelFile (BSC.unpack entryName) >>= ZIP.mkEntrySelector)
  return
    (do
       ZIP.sinkEntry ZIP.Store (CND.sourceFileBS (BSC.unpack f)) entrySelector
       ZIP.setEntryComment (TE.decodeUtf8 $ BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) viewAsset (HTML, assetId ar) []) entrySelector)

containerZipEntry2 :: Bool -> BS.ByteString -> Container -> [AssetSlot] -> Handler (ZIP.ZipArchive ())
containerZipEntry2 isOrig prefix c l = do
  let containerDir = prefix <> makeFilename (containerDownloadName c) <> "/"
  zipActs <- mapM (assetZipEntry2 isOrig containerDir) l
  return (sequence_ zipActs)

volumeDescription :: Bool -> Volume -> (Container, [RecordSlot]) -> IdSet Container -> [AssetSlot] -> Handler (Html.Html, [[AssetSlot]], [[AssetSlot]])
volumeDescription inzip v (_, glob) cs al = do
  cite <- lookupVolumeCitation v
  links <- lookupVolumeLinks v
  fund <- lookupVolumeFunding v
  desc <- peeks $ htmlVolumeDescription inzip v (maybeToList cite ++ links) fund glob cs at ab
  return (desc, at, ab)
  where
  (at, ab) = partition (any (containerTop . containerRow . slotContainer) . assetSlot . head) $ groupBy (me `on` fmap (containerId . containerRow . slotContainer) . assetSlot) al
  me (Just x) (Just y) = x == y
  me _ _ = False

volumeZipEntry2 :: Bool -> Volume -> (Container, [RecordSlot]) -> IdSet Container -> Maybe BSB.Builder -> [AssetSlot] -> Handler (ZIP.ZipArchive ())
volumeZipEntry2 isOrig v top cs csv al = do
  (desc, at, ab) <- volumeDescription True v top cs al -- the actual asset slot's assets arent' used any more for containers, now container zip entry does that
  let zipDir = makeFilename (volumeDownloadName v ++ if idSetIsFull cs then [] else ["PARTIAL"]) <> "/"
  zt <- mapM (ent zipDir) at
  zb <- mapM (ent (zipDir <> "sessions/")) ab
  descEntrySelector <- liftIO (parseRelFile (BSC.unpack zipDir <> "description.html") >>= ZIP.mkEntrySelector)
  spreadEntrySelector <- liftIO (parseRelFile (BSC.unpack zipDir <> "spreadsheet.csv") >>= ZIP.mkEntrySelector)
  return
    (do
       sequence_ zt
       sequence_ zb
       ZIP.addEntry ZIP.Store (BSL.toStrict (Html.renderHtml desc)) descEntrySelector
       maybe (pure ()) (\c -> ZIP.addEntry ZIP.Store (BSL.toStrict (BSB.toLazyByteString c)) spreadEntrySelector) csv)
  where
  ent prefix [a@AssetSlot{ assetSlot = Nothing }] = assetZipEntry2 isOrig prefix a -- orig asset doesn't matter here as top level assets aren't transcoded, I believe
  ent prefix (AssetSlot{ assetSlot = Just s } : _) = do
    (acts, _) <- containerZipEntryCorrectAssetSlots2 isOrig prefix (slotContainer s)
    pure acts
  ent _ _ = fail "volumeZipEntry"

zipResponse2 :: BS.ByteString -> ZIP.ZipArchive () -> Handler Response
zipResponse2 n zipAddActions = do
  req <- peek
  u <- peek
  store <- peek
  let comment = BSL.toStrict $ BSB.toLazyByteString
        $ BSB.string8 "Downloaded by " <> TE.encodeUtf8Builder (partyName $ partyRow u) <> BSB.string8 " <" <> actionURL (Just req) viewParty (HTML, TargetParty $ partyId $ partyRow u) [] <> BSB.char8 '>'
  let temporaryZipName = (BSC.unpack . storageTemp) store <> "placeholder.zip" -- TODO: generate temporary name for extra caution?
  h <- liftIO $ IO.openFile temporaryZipName IO.ReadWriteMode
  liftIO $ DIR.removeFile temporaryZipName
  liftIO $ IO.hSetBinaryMode h True
  liftIO $ ZIP.createBlindArchive h $ do
    ZIP.setArchiveComment (TE.decodeUtf8 comment)
    zipAddActions
  sz <- liftIO (IO.hSeek h IO.SeekFromEnd 0 >> IO.hTell h)
  liftIO $ IO.hSeek h IO.AbsoluteSeek 0
  return $ okResponse
    [ (hContentType, "application/zip")
    , ("content-disposition", "attachment; filename=" <> quoteHTTP (n <.> "zip"))
    , (hCacheControl, "max-age=31556926, private")
    , (hContentLength, BSC.pack $ show sz)
    ] (CND.bracketP (return h) IO.hClose CND.sourceHandle :: CND.Source (CND.ResourceT IO) BS.ByteString)

checkAsset :: AssetSlot -> Bool
checkAsset a =
  canReadData2 getAssetSlotRelease2 getAssetSlotVolumePermission2 a && assetBacked (view a)

containerZipEntryCorrectAssetSlots2 :: Bool -> BS.ByteString -> Container -> Handler (ZIP.ZipArchive (), Bool)
containerZipEntryCorrectAssetSlots2 isOrig prefix c = do
  c'<- lookupContainerAssets c
  assetSlots <- case isOrig of
                     True -> do
                      origs <- lookupOrigContainerAssets c
                      let pdfs = filterFormat c' formatNotAV
                      return $ pdfs ++ origs
                     False -> return c'
  let checkedAssetSlots = filter checkAsset assetSlots
  zipActs <- containerZipEntry2 isOrig prefix c checkedAssetSlots
  pure (zipActs, null checkedAssetSlots)

zipContainer :: Bool -> ActionRoute (Maybe (Id Volume), Id Slot)
zipContainer isOrig =
  let zipPath = case isOrig of
                     True -> pathMaybe pathId </> pathSlotId </< "zip" </< "true"
                     False -> pathMaybe pathId </> pathSlotId </< "zip" </< "false"
  in action GET zipPath $ \(vi, ci) -> withAuth $ do
    c <- getContainer PermissionPUBLIC vi ci True
    let v = containerVolume c
    _ <- maybeAction (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted
    (zipActs, isEmpty) <- containerZipEntryCorrectAssetSlots2 isOrig "" c
    auditSlotDownload (not isEmpty) (containerSlot c)
    zipResponse2 ("databrary-" <> BSC.pack (show $ volumeId $ volumeRow $ containerVolume c) <> "-" <> BSC.pack (show $ containerId $ containerRow c)) zipActs

getVolumeInfo :: Id Volume -> Handler (Volume, IdSet Container, [AssetSlot])
getVolumeInfo vi = do
  v <- getVolume PermissionPUBLIC vi
  _ <- maybeAction (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted
  s <- peeks requestIdSet
  -- let isMember = maybe (const False) (\c -> RS.member (containerId $ containerRow $ slotContainer $ c))
  -- non-exhaustive pattern found here ...v , implement in case of Nothing (Keep in mind originalAssets will not have containers, or Volumes)
  a <- filter (\a@AssetSlot{ assetSlot = Just c } -> checkAsset a && RS.member (containerId $ containerRow $ slotContainer c) s) <$> lookupVolumeAssetSlots v False
  return (v, s, a)

filterFormat :: [AssetSlot] -> (Format -> Bool)-> [AssetSlot]
filterFormat as f = filter (f . assetFormat . assetRow . slotAsset ) as

zipVolume :: Bool -> ActionRoute (Id Volume)
zipVolume isOrig =
  let zipPath = case isOrig of
                     True -> pathId </< "zip" </< "true"
                     False -> pathId </< "zip" </< "false"
  in action GET zipPath $ \vi -> withAuth $ do
  (v, s, a) <- getVolumeInfo vi
  top:cr <- lookupVolumeContainersRecords v
  let cr' = filter ((`RS.member` s) . containerId . containerRow . fst) cr
  csv <- null cr' `unlessReturn` volumeCSV v cr'
  zipActs <- volumeZipEntry2 isOrig v top s (buildCSV <$> csv) a
  auditVolumeDownload (not $ null a) v
  zipResponse2 (BSC.pack $ "databrary-" ++ show (volumeId $ volumeRow v) ++ if idSetIsFull s then "" else "-partial") zipActs

viewVolumeDescription :: ActionRoute (Id Volume)
viewVolumeDescription = action GET (pathId </< "description") $ \_ -> withAuth $ do
  angular
  {-
  (v, s, a) <- getVolumeInfo vi
  top <- lookupVolumeTopContainer v
  glob <- lookupSlotRecords $ containerSlot top
  (desc, _, _) <- volumeDescription False v (top, glob) s a
  -}
  return $ okResponse [] (""::String)
