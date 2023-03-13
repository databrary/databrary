{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Routes.JS
  ( jsRoutes
  , fakeBackendDepend1
  , fakeBackendDepend2
  , fakeBackendDepend3
  , fakeBackendDepend4
  ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import Data.Monoid ((<>))

import Model.Record (Record)
import Model.Asset (Asset)
import Model.Volume (Volume)
import Model.Party (Party)
import Model.Id.Types
import Model.Token (LoginToken)
import Model.Segment
import Model.Slot.Types
import Action
import Controller.Paths
import Controller.Root
import Controller.Login
import Controller.Register
import Controller.Token
import Controller.Party
import Controller.Volume
-- import Controller.Container
import Controller.Slot
import Controller.Record
import Controller.Format
import Controller.Asset
import Controller.AssetSegment
import Controller.Zip
import Controller.CSV
import Controller.Search
import Controller.Activity

rt :: BS.ByteString -> B.Builder
rt routeKeyVal = B.byteString ("\n" <> routeKeyVal)

jsRoutes :: [B.Builder] -- should be mconcat, but BSB bug causes hangs
jsRoutes =
  [ rt "\"viewRoot\":{method:\"GET\",route:function(){return \"/\";}},"
   -- jsRoute "viewRoot" viewRoot (HTML)
  , rt "\"viewLogin\":{method:\"GET\",route:function(){return \"/user/login\";}},"
   -- , jsRoute "viewLogin" viewLogin ()
  , rt "\"viewRegister\":{method:\"GET\",route:function(){return \"/user/register\";}},"
  -- , jsRoute "viewRegister" viewRegister ()
  , rt "\"viewPasswordReset\":{method:\"GET\",route:function(){return \"/user/password\";}},"
  -- , jsRoute "viewPasswordReset" viewPasswordReset ()
  , rt "\"viewLoginToken\":{method:\"GET\",route:function(byteString0){return \"/token/\"+byteString0+\"\";}},"
  -- , jsRoute "viewLoginToken" viewLoginToken (HTML, token)

  , rt "\"viewProfile\":{method:\"GET\",route:function(){return \"/profile\";}},"
  -- , jsRoute "viewProfile" viewParty (HTML, TargetProfile)
  , rt "\"viewProfileEdit\":{method:\"GET\",route:function(){return \"/profile/edit\";}},"
  -- , jsRoute "viewProfileEdit" viewPartyEdit (TargetProfile)
  , rt "\"viewParty\":{method:\"GET\",route:function(int320){return \"/party/\"+int320+\"\";}},"
  -- , jsRoute "viewParty" viewParty (HTML, TargetParty party)
  , rt "\"viewPartyEdit\":{method:\"GET\",route:function(int320){return \"/party/\"+int320+\"/edit\";}},"
  -- , jsRoute "viewPartyEdit" viewPartyEdit (TargetParty party)
  , rt "\"viewPartySearch\":{method:\"GET\",route:function(){return \"/party\";}},"
  -- , jsRoute "viewPartySearch" queryParties (HTML)
  , rt "\"partyAvatar\":{method:\"GET\",route:function(int320){return \"/party/\"+int320+\"/avatar\";}},"
  -- , jsRoute "partyAvatar" viewAvatar (party)
  , rt "\"viewPartyActivity\":{method:\"GET\",route:function(int320){return \"/party/\"+int320+\"/activity\";}},"
  -- , jsRoute "viewPartyActivity" viewPartyActivity (HTML, TargetParty party)

  , rt "\"viewVolume\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"\";}},"
  -- , jsRoute "viewVolume" viewVolume (HTML, volume)
  , rt "\"viewVolumeCreate\":{method:\"GET\",route:function(){return \"/volume/create\";}},"
  -- , jsRoute "viewVolumeCreate" viewVolumeCreate ()
  , rt "\"viewVolumeEdit\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"/edit\";}},"
  -- , jsRoute "viewVolumeEdit" viewVolumeEdit (volume)
  , rt "\"viewVolumeSearch\":{method:\"GET\",route:function(){return \"/volume\";}},"
  -- , jsRoute "viewVolumeSearch" queryVolumes (HTML)
  , rt "\"thumbVolume\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"/thumb\";}},"
  -- , jsRoute "thumbVolume" thumbVolume (volume)
  , rt "\"csvVolume\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"/csv\";}},"
  -- , jsRoute "csvVolume" csvVolume (volume)
  , rt "\"viewVolumeActivity\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"/activity\";}},"
  -- , jsRoute "viewVolumeActivity" viewVolumeActivity (HTML, volume)

  , rt "\"viewSlot\":{method:\"GET\",route:function(int320,int321,segment2){return \"/volume/\"+int320+\"/slot/\"+int321+\"/\"+segment2+\"\";}},"
  -- , jsRoute "viewSlot" (viewSlot False) (HTML, (Just volume, slot))
  , rt "\"viewSlotEdit\":{method:\"GET\",route:function(int320,int321){return \"/volume/\"+int320+\"/slot/\"+int321+\"/edit\";}},"
  -- , jsRoute "viewSlotEdit" viewContainerEdit (Just volume, container)
  , rt "\"viewSlotActivity\":{method:\"GET\",route:function(int320,int321){return \"/volume/\"+int320+\"/slot/\"+int321+\"/activity\";}},"
  -- , jsRoute "viewSlotActivity" viewContainerActivity (HTML, (Just volume, container))
  , rt "\"thumbSlot\":{method:\"GET\",route:function(int320,int321,segment2){return \"/volume/\"+int320+\"/slot/\"+int321+\"/\"+segment2+\"/thumb\";}},"
  -- , jsRoute "thumbSlot" thumbSlot (Just volume, slot)

  , rt "\"viewRecord\":{method:\"GET\",route:function(int320){return \"/record/\"+int320+\"\";}},"
  -- , jsRoute "viewRecord" viewRecord (record)

  , rt "\"viewFormats\":{method:\"GET\",route:function(){return \"/asset/formats\";}},"
  -- , jsRoute "viewFormats" viewFormats ()
  , rt "\"viewAssetSegment\":{method:\"GET\",route:function(int320,int321,segment2,int323){return \"/volume/\"+int320+\"/slot/\"+int321+\"/\"+segment2+\"/asset/\"+int323+\"\";}},"
  -- , jsRoute "viewAssetSegment" (viewAssetSegment False) (HTML, Just volume, slot, asset)
  , rt "\"viewOrigAssetSegment\":{method:\"GET\",route:function(int320,int321,segment2,int323){return \"/volume/\"+int320+\"/slot/\"+int321+\"/\"+segment2+\"/asset/\"+int323+\"\";}},"
  -- , jsRoute "viewOrigAssetSegment" (viewAssetSegment True) (HTML, Just volume, slot, asset)
  , rt "\"downloadAssetSegment\":{method:\"GET\",route:function(int320,segment1,int322){return \"/slot/\"+int320+\"/\"+segment1+\"/asset/\"+int322+\"/download\";}},"
  -- , jsRoute "downloadAssetSegment" downloadAssetSegment (slot, asset) --download transcoded asset segment
  , rt "\"downloadOrigAssetSegment\":{method:\"GET\",route:function(int320,segment1,int322){return \"/slot/\"+int320+\"/\"+segment1+\"/asset/\"+int322+\"/downloadOrig\";}},"
  -- , jsRoute "downloadOrigAssetSegment" downloadOrigAssetSegment (slot, asset) --download original asset segment
  , rt "\"thumbAssetSegment\":{method:\"GET\",route:function(int320,segment1,int322){return \"/slot/\"+int320+\"/\"+segment1+\"/asset/\"+int322+\"/thumb\";}},"
  -- , jsRoute "thumbAssetSegment" (thumbAssetSegment False) (slot, asset)
  , rt "\"thumbOrigAssetSegment\":{method:\"GET\",route:function(int320,segment1,int322){return \"/slot/\"+int320+\"/\"+segment1+\"/asset/\"+int322+\"/thumb\";}},"
  -- , jsRoute "thumbOrigAssetSegment" (thumbAssetSegment True) (slot, asset)
  , rt "\"downloadAsset\":{method:\"GET\",route:function(int320){return \"/asset/\"+int320+\"/download\";}},"
  -- , jsRoute "downloadAsset" downloadAsset (asset, fullSegment)
  , rt "\"downloadOrigAsset\":{method:\"GET\",route:function(int320){return \"/asset/\"+int320+\"/downloadOrig\";}},"
  -- , jsRoute "downloadOrigAsset" downloadOrigAsset (asset, fullSegment) --download original asset
  , rt "\"thumbAsset\":{method:\"GET\",route:function(int320){return \"/asset/\"+int320+\"/thumb\";}},"
  -- , jsRoute "thumbAsset" thumbAsset (asset, fullSegment)

  , rt "\"viewSearch\":{method:\"GET\",route:function(){return \"/search\";}},"
  -- , jsRoute "viewSearch" postSearch (HTML)

  , rt "\"zipSlot\":{method:\"GET\",route:function(int320,int321){return \"/volume/\"+int320+\"/slot/\"+int321+\"/zip/false\";}},"
  -- , jsRoute "zipSlot" (zipContainer False) (Just volume, container) --zip transcoded slot
  , rt "\"zipOrigSlot\":{method:\"GET\",route:function(int320,int321){return \"/volume/\"+int320+\"/slot/\"+int321+\"/zip/true\";}},"
  -- , jsRoute "zipOrigSlot" (zipContainer True) (Just volume, container) --zip original slot
  , rt "\"zipVolume\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"/zip/false\";}},"
  -- , jsRoute "zipVolume" (zipVolume False) (volume) -- zip volume full of transcoded assets
  , rt "\"zipOrigVolume\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"/zip/true\";}},"
  -- , jsRoute "zipOrigVolume" (zipVolume True) (volume) -- zip volume full of original assets
  , rt "\"viewVolumeDescription\":{method:\"GET\",route:function(int320){return \"/volume/\"+int320+\"/description\";}},"
  -- , jsRoute "viewVolumeDescription" viewVolumeDescription (volume)

  , rt "\"get\":{method:\"GET\",route:function(){return \"/api\";}},"
  -- , jsRoute "get" viewRoot (JSON)
  , rt "\"getUser\":{method:\"GET\",route:function(){return \"/api/user\";}},"
  -- , jsRoute "getUser" viewUser ()
  , rt "\"postUser\":{method:\"POST\",route:function(){return \"/api/user\";}},"
  -- , jsRoute "postUser" postUser (JSON)
  , rt "\"postLogin\":{method:\"POST\",route:function(){return \"/api/user/login\";}},"
  -- , jsRoute "postLogin" postLogin (JSON)
  , rt "\"postLogout\":{method:\"POST\",route:function(){return \"/api/user/logout\";}},"
  -- , jsRoute "postLogout" postLogout (JSON)
  , rt "\"postRegister\":{method:\"POST\",route:function(){return \"/api/user/register\";}},"
  -- , jsRoute "postRegister" postRegister (JSON)
  , rt "\"postPasswordReset\":{method:\"POST\",route:function(){return \"/api/user/password\";}},"
  -- , jsRoute "postPasswordReset" postPasswordReset (JSON)
  , rt "\"getLoginToken\":{method:\"GET\",route:function(byteString0){return \"/api/token/\"+byteString0+\"\";}},"
  -- , jsRoute "getLoginToken" viewLoginToken (JSON, token)
  , rt "\"postPasswordToken\":{method:\"POST\",route:function(byteString0){return \"/api/token/\"+byteString0+\"\";}},"
  -- , jsRoute "postPasswordToken" postPasswordToken (JSON, token)

  , rt "\"getParty\":{method:\"GET\",route:function(int320){return \"/api/party/\"+int320+\"\";}},"
  -- , jsRoute "getParty" viewParty (JSON, TargetParty party)
  , rt "\"getProfile\":{method:\"GET\",route:function(){return \"/api/profile\";}},"
  -- , jsRoute "getProfile" viewParty (JSON, TargetProfile)
  , rt "\"postParty\":{method:\"POST\",route:function(int320){return \"/api/party/\"+int320+\"\";}},"
  -- , jsRoute "postParty" postParty (JSON, TargetParty party)
  , rt "\"getParties\":{method:\"GET\",route:function(){return \"/api/party\";}},"
  -- , jsRoute "getParties" queryParties (JSON)
  , rt "\"getPartyActivity\":{method:\"GET\",route:function(int320){return \"/api/party/\"+int320+\"/activity\";}},"
  -- , jsRoute "getPartyActivity" viewPartyActivity (JSON, TargetParty party)

  , rt "\"postAuthorizeApply\":{method:\"POST\",route:function(int320,int321){return \"/api/party/\"+int320+\"/apply/\"+int321+\"\";}},"
  -- , jsRoute "postAuthorizeApply" postAuthorize (JSON, TargetParty party, AuthorizeTarget True party)
  , rt "\"postAuthorize\":{method:\"POST\",route:function(int320,int321){return \"/api/party/\"+int320+\"/authorize/\"+int321+\"\";}},"
  -- , jsRoute "postAuthorize" postAuthorize (JSON, TargetParty party, AuthorizeTarget False party)
  , rt "\"deleteAuthorize\":{method:\"DELETE\",route:function(int320,int321){return \"/api/party/\"+int320+\"/authorize/\"+int321+\"\";}},"
  -- , jsRoute "deleteAuthorize" deleteAuthorize (JSON, TargetParty party, AuthorizeTarget False party)
  , rt "\"deleteAuthorizeParent\":{method:\"DELETE\",route:function(int320,int321){return \"/api/party/\"+int320+\"/apply/\"+int321+\"\";}},"
  -- , jsRoute "deleteAuthorizeParent" deleteAuthorize (JSON, TargetParty party, AuthorizeTarget True party)
  , rt "\"postAuthorizeNotFound\":{method:\"POST\",route:function(int320){return \"/api/party/\"+int320+\"/notfound\";}},"
  -- , jsRoute "postAuthorizeNotFound" postAuthorizeNotFound (TargetParty party)

  , rt "\"getVolume\":{method:\"GET\",route:function(int320){return \"/api/volume/\"+int320+\"\";}},"
  -- , jsRoute "getVolume" viewVolume (JSON, volume)
  , rt "\"postVolume\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"\";}},"
  -- , jsRoute "postVolume" postVolume (JSON, volume)
  , rt "\"createVolume\":{method:\"POST\",route:function(){return \"/api/volume\";}},"
  -- , jsRoute "createVolume" createVolume (JSON)
  , rt "\"getVolumes\":{method:\"GET\",route:function(){return \"/api/volume\";}},"
  -- , jsRoute "getVolumes" queryVolumes (JSON)
  , rt "\"postVolumeAccess\":{method:\"POST\",route:function(int320,int321){return \"/api/volume/\"+int320+\"/access/\"+int321+\"\";}},"
  -- , jsRoute "postVolumeAccess" postVolumeAccess (JSON, (volume, VolumeAccessTarget party))
  , rt "\"postVolumeFunding\":{method:\"POST\",route:function(int320,int641){return \"/api/volume/\"+int320+\"/funder/\"+int641+\"\";}},"
  -- , jsRoute "postVolumeFunding" postVolumeFunding (volume, funder)
  , rt "\"postVolumeLinks\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/link\";}},"
  -- , jsRoute "postVolumeLinks" postVolumeLinks (JSON, volume)
  , rt "\"deleteVolumeFunder\":{method:\"DELETE\",route:function(int320,int641){return \"/api/volume/\"+int320+\"/funder/\"+int641+\"\";}},"
  -- , jsRoute "deleteVolumeFunder" deleteVolumeFunder (volume, funder)
  , rt "\"postVolumeAssist\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/assist\";}},"
  -- , jsRoute "postVolumeAssist" postVolumeAssist (volume)
  , rt "\"getVolumeActivity\":{method:\"GET\",route:function(int320){return \"/api/volume/\"+int320+\"/activity\";}},"
  -- , jsRoute "getVolumeActivity" viewVolumeActivity (JSON, volume)

  , rt "\"postSearch\":{method:\"GET\",route:function(){return \"/api/search\";}},"
  -- , jsRoute "postSearch" postSearch (JSON)
  , rt "\"getFunders\":{method:\"GET\",route:function(){return \"/api/funder\";}},"
  -- , jsRoute "getFunders" queryFunder ()
  , rt "\"getCitation\":{method:\"GET\",route:function(){return \"/api/cite\";}},"
  -- , jsRoute "getCitation" getCitation ()

  , rt "\"getSlot\":{method:\"GET\",route:function(int320,segment1){return \"/api/slot/\"+int320+\"/\"+segment1+\"\";}},"
  -- , jsRoute "getSlot" (viewSlot False) (JSON, (Nothing, slot))
  , rt "\"postContainer\":{method:\"POST\",route:function(int320){return \"/api/slot/\"+int320+\"\";}},"
  -- , jsRoute "postContainer" postContainer (container)
  , rt "\"deleteContainer\":{method:\"DELETE\",route:function(int320){return \"/api/slot/\"+int320+\"\";}},"
  -- , jsRoute "deleteContainer" deleteContainer (container)
  , rt "\"createContainer\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/slot\";}},"
  -- , jsRoute "createContainer" createContainer (volume)
  , rt "\"getContainerActivity\":{method:\"GET\",route:function(int320){return \"/api/slot/\"+int320+\"/activity\";}},"
  -- , jsRoute "getContainerActivity" viewContainerActivity (JSON, (Nothing, container))

  , rt "\"getRecord\":{method:\"GET\",route:function(int320){return \"/api/record/\"+int320+\"\";}},"
  -- , jsRoute "getRecord" viewRecord (record)
  , rt "\"createRecord\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/record\";}},"
  -- , jsRoute "createRecord" createRecord (JSON, volume)
  , rt "\"deleteRecord\":{method:\"DELETE\",route:function(int320){return \"/api/record/\"+int320+\"\";}},"
  -- , jsRoute "deleteRecord" deleteRecord (JSON, record)
  , rt "\"postRecordMeasure\":{method:\"POST\",route:function(int320,int321){return \"/api/record/\"+int320+\"/metric/\"+int321+\"\";}},"
  -- , jsRoute "postRecordMeasure" postRecordMeasure (JSON, record, metric)
  , rt "\"postRecordSlot\":{method:\"POST\",route:function(int320,segment1,int322){return \"/api/slot/\"+int320+\"/\"+segment1+\"/record/\"+int322+\"\";}},"
  -- , jsRoute "postRecordSlot" postRecordSlot (JSON, slot, record)
  , rt "\"deleteRecordAllSlot\":{method:\"DELETE\",route:function(int320){return \"/api/slot/all/record/\"+int320+\"\";}},"
  -- , jsRoute "deleteRecordAllSlot" deleteRecordAllSlot (JSON, record)

  , rt "\"addVolumeMetric\":{method:\"PUT\",route:function(int320,int321){return \"/api/volume/\"+int320+\"/metric/\"+int321+\"\";}},"
  -- , jsRoute "addVolumeMetric" postVolumeMetric (volume, Right metric)
  , rt "\"addVolumeCategory\":{method:\"PUT\",route:function(int320,int161){return \"/api/volume/\"+int320+\"/category/\"+int161+\"\";}},"
  -- , jsRoute "addVolumeCategory" postVolumeMetric (volume, Left category)
  , rt "\"deleteVolumeMetric\":{method:\"DELETE\",route:function(int320,int321){return \"/api/volume/\"+int320+\"/metric/\"+int321+\"\";}},"
  -- , jsRoute "deleteVolumeMetric" deleteVolumeMetric (volume, Right metric)
  , rt "\"deleteVolumeCategory\":{method:\"DELETE\",route:function(int320,int161){return \"/api/volume/\"+int320+\"/category/\"+int161+\"\";}},"
  -- , jsRoute "deleteVolumeCategory" deleteVolumeMetric (volume, Left category)
  , rt "\"postVolumeState\":{method:\"PUT\",route:function(int320,text1){return \"/api/volume/\"+int320+\"/state/\"+text1+\"\";}},"
  -- , jsRoute "postVolumeState" postVolumeState (volume, "")
  , rt "\"deleteVolumeState\":{method:\"DELETE\",route:function(int320,text1){return \"/api/volume/\"+int320+\"/state/\"+text1+\"\";}},"
  -- , jsRoute "deleteVolumeState" deleteVolumeState (volume, "")

  , rt "\"getAsset\":{method:\"GET\",route:function(int320){return \"/api/asset/\"+int320+\"\";}},"
  -- , jsRoute "getAsset" viewAsset (JSON, asset)
  , rt "\"getAssetSegment\":{method:\"GET\",route:function(int320,int321,segment2,int323){return \"/api/volume/\"+int320+\"/slot/\"+int321+\"/\"+segment2+\"/asset/\"+int323+\"\";}},"
  -- , jsRoute "getAssetSegment" (viewAssetSegment False) (JSON, Just volume, slot, asset)
  , rt "\"postAsset\":{method:\"POST\",route:function(int320){return \"/api/asset/\"+int320+\"\";}},"
  -- , jsRoute "postAsset" postAsset (asset)
  , rt "\"createAsset\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/asset\";}},"
  -- , jsRoute "createAsset" createAsset (volume)
  , rt "\"deleteAsset\":{method:\"DELETE\",route:function(int320){return \"/api/asset/\"+int320+\"\";}},"
  -- , jsRoute "deleteAsset" deleteAsset (asset)
  , rt "\"postExcerpt\":{method:\"POST\",route:function(int320,segment1,int322){return \"/api/slot/\"+int320+\"/\"+segment1+\"/asset/\"+int322+\"/excerpt\";}},"
  -- , jsRoute "postExcerpt" postExcerpt (slot, asset)
  , rt "\"deleteExcerpt\":{method:\"DELETE\",route:function(int320,segment1,int322){return \"/api/slot/\"+int320+\"/\"+segment1+\"/asset/\"+int322+\"/excerpt\";}},"
  -- , jsRoute "deleteExcerpt" deleteExcerpt (slot, asset)
  , rt "\"uploadStart\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/upload\";}},"
  -- , jsRoute "uploadStart" uploadStart (volume)
  , rt "\"uploadChunk\":{method:\"POST\",route:function(){return \"/api/upload\";}},"
  -- , jsRoute "uploadChunk" uploadChunk ()

  , rt "\"postComment\":{method:\"POST\",route:function(int320,segment1){return \"/api/slot/\"+int320+\"/\"+segment1+\"/comment\";}},"
  -- , jsRoute "postComment" postComment (slot)
  , rt "\"getTags\":{method:\"GET\",route:function(tagName0){return \"/api/tags/\"+tagName0+\"\";}},"
  -- , jsRoute "getTags" queryTags (Just tag)
  , rt "\"postTag\":{method:\"POST\",route:function(int320,segment1,tagName2){return \"/api/slot/\"+int320+\"/\"+segment1+\"/tag/\"+tagName2+\"\";}},"
  -- , jsRoute "postTag" postTag (JSON, slot, TagId False tag)
  , rt "\"postKeyword\":{method:\"POST\",route:function(int320,segment1,tagName2){return \"/api/slot/\"+int320+\"/\"+segment1+\"/keyword/\"+tagName2+\"\";}},"
  -- , jsRoute "postKeyword" postTag (JSON, slot, TagId True tag)
  , rt "\"deleteTag\":{method:\"DELETE\",route:function(int320,segment1,tagName2){return \"/api/slot/\"+int320+\"/\"+segment1+\"/tag/\"+tagName2+\"\";}},"
  -- , jsRoute "deleteTag" deleteTag (JSON, slot, TagId False tag)
  , rt "\"deleteKeyword\":{method:\"DELETE\",route:function(int320,segment1,tagName2){return \"/api/slot/\"+int320+\"/\"+segment1+\"/keyword/\"+tagName2+\"\";}},"
  -- , jsRoute "deleteKeyword" deleteTag (JSON, slot, TagId True tag)
  , rt "\"getTopTags\":{method:\"GET\",route:function(){return \"/api/tags\";}},"
  -- , jsRoute "getTopTags" queryTags Nothing
  , rt "\"getSiteActivity\":{method:\"GET\",route:function(){return \"/api/activity\";}},"
  -- , jsRoute "getSiteActivity" viewSiteActivity ()

  , rt "\"getNotifications\":{method:\"GET\",route:function(){return \"/api/notification\";}},"
  -- , jsRoute "getNotifications" viewNotifications ()
  , rt "\"deleteNotification\":{method:\"DELETE\",route:function(int320){return \"/api/notification/\"+int320+\"\";}},"
  -- , jsRoute "deleteNotification" deleteNotification (Id 0)
  , rt "\"deleteNotifications\":{method:\"DELETE\",route:function(){return \"/api/notification\";}},"
  -- , jsRoute "deleteNotifications" deleteNotifications ()
  , rt "\"getNotify\":{method:\"GET\",route:function(){return \"/api/notify\";}},"
  -- , jsRoute "getNotify" viewNotify ()
  , rt "\"postNotify\":{method:\"POST\",route:function(){return \"/api/notify\";}},"
  -- , jsRoute "postNotify" postNotify ()

  , rt "\"detectParticipantCSV\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/detectParticipantCSV\";}},"
    -- , jsRoute "detectParticipantCSV" detectParticipantCSV (JSON, volume)
  , rt "\"runParticipantUpload\":{method:\"POST\",route:function(int320){return \"/api/volume/\"+int320+\"/runParticipantUpload\";}},"
    -- , jsRoute "runParticipantUpload" runParticipantUpload (JSON, volume)
  ]

-- This type decl generated by copying out the text from GHC's warning message,
-- then formatting with brittany.
fakeBackendDepend1
    :: ( ActionRoute API
       , ActionRoute ()
       , ActionRoute ()
       , ActionRoute ()
       , ActionRoute (API, Id LoginToken)
       , ActionRoute (API, PartyTarget)
       , ActionRoute PartyTarget
       , ActionRoute API
       , ActionRoute (Id Party)
       , ActionRoute (API, PartyTarget)
       , ActionRoute (API, Id Volume)
       , Action
       , ActionRoute (Id Volume)
       , ActionRoute API
       , ActionRoute (Id Volume)
       , ActionRoute (Id Volume)
       , ActionRoute (API, Id Volume)
       , Bool -> ActionRoute (API, (Maybe (Id Volume), Id Slot))
       , ActionRoute (API, (Maybe (Id Volume), Id Slot))
       , ActionRoute (Maybe (Id Volume), Id Slot)
       , ActionRoute (Id Record)
       )
fakeBackendDepend1 =
  ( viewRoot :: ActionRoute API
  , viewLogin :: ActionRoute ()
  , viewRegister :: ActionRoute ()
  , viewPasswordReset :: ActionRoute ()
  , viewLoginToken :: ActionRoute (API, Id LoginToken)
  , viewParty :: ActionRoute (API, PartyTarget)
  , viewPartyEdit :: ActionRoute PartyTarget
  , queryParties :: ActionRoute API
  , viewAvatar :: ActionRoute (Id Party)
  , viewPartyActivity :: ActionRoute (API, PartyTarget)
  , viewVolume :: ActionRoute (API, Id Volume)
  , viewVolumeCreateHandler :: Action
  , viewVolumeEdit :: ActionRoute (Id Volume)
  , queryVolumes :: ActionRoute API
  , thumbVolume :: ActionRoute (Id Volume)
  , csvVolume :: ActionRoute (Id Volume)
  , viewVolumeActivity :: ActionRoute (API, Id Volume)
  , viewSlot :: Bool -> ActionRoute (API, (Maybe (Id Volume), Id Slot))
  , viewContainerActivity :: ActionRoute (API, (Maybe (Id Volume), Id Slot))
  , thumbSlot :: ActionRoute (Maybe (Id Volume), Id Slot)
  , viewRecord :: ActionRoute (Id Record)
  )

-- This type decl generated by copying out the text from GHC's warning message,
-- then formatting with brittany.
fakeBackendDepend2
    :: ( Action
       , Bool -> ActionRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
       , ActionRoute (Id Slot, Id Asset)
       , ActionRoute (Id Slot, Id Asset)
       , Bool -> ActionRoute (Id Slot, Id Asset)
       , ActionRoute (Id Asset, Segment)
       , ActionRoute (Id Asset, Segment)
       , ActionRoute (Id Asset, Segment)
       , API -> Action
       , Bool -> ActionRoute (Maybe (Id Volume), Id Slot)
       , Bool -> ActionRoute (Id Volume)
       , ActionRoute (Id Volume)
       , ActionRoute API
       , API -> [(BS.ByteString, BS.ByteString)] -> Action
       , ActionRoute API
       , ActionRoute API
       , ActionRoute API
       , ActionRoute API
       , ActionRoute (API, Id LoginToken)
       , ActionRoute (API, Id LoginToken)
       )
fakeBackendDepend2 =
  ( viewFormatsHandler :: Action
  , viewAssetSegment :: Bool -> ActionRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
  , downloadAssetSegment :: ActionRoute (Id Slot, Id Asset)
  , downloadOrigAssetSegment :: ActionRoute (Id Slot, Id Asset)
  , thumbAssetSegment :: Bool -> ActionRoute (Id Slot, Id Asset)
  , downloadAsset :: ActionRoute (Id Asset, Segment)
  , downloadOrigAsset :: ActionRoute (Id Asset, Segment)
  , thumbAsset :: ActionRoute (Id Asset, Segment)
  , postSearchHandler :: API -> Action
  , zipContainer :: Bool -> ActionRoute (Maybe (Id Volume), Id Slot)
  , zipVolume :: Bool -> ActionRoute (Id Volume)
  , viewVolumeDescription :: ActionRoute (Id Volume)
  , viewRoot :: ActionRoute API
  , userHandler :: API -> [(BS.ByteString, BS.ByteString)] -> Action
  -- , postUser :: ActionRoute API
  , postLogin :: ActionRoute API
  , postLogout :: ActionRoute API
  , postRegister :: ActionRoute API
  , postPasswordReset :: ActionRoute API
  , viewLoginToken :: ActionRoute (API, Id LoginToken)
  , postPasswordToken :: ActionRoute (API, Id LoginToken)
  -- TODO: finish enumerating
  )

fakeBackendDepend3, fakeBackendDepend4 :: ()
fakeBackendDepend3 = ()
fakeBackendDepend4 = ()
