{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.Container
  ( module Databrary.Model.Container.Types
  , blankContainer
  , lookupContainer
  , lookupVolumeContainer
  , lookupVolumeContainers
  , lookupVolumeTopContainer
  , containerIsVolumeTop
  , addContainer
  , changeContainer
  , removeContainer
  , getContainerDate
  , formatContainerDate
  , containerRowJSON
  , containerJSON
  ) where

import Control.Monad (guard)
import Data.Either (isRight)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Ops
import Databrary.Has (view, peek)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery, isForeignKeyViolation)
import Databrary.Model.Time
import Databrary.Model.Permission
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Identity
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.PermissionUtil (maskRestrictedString)

blankContainer :: Volume -> Container
blankContainer vol = Container
  { containerRow = ContainerRow
    { containerId = error "blankContainer"
    , containerTop = False
    , containerName = Nothing
    , containerDate = Nothing
    }
  , containerRelease = Nothing
  , containerVolume = vol
  }

lookupContainer :: (MonadDB c m, MonadHasIdentity c m) => Id Container -> m (Maybe Container)
lookupContainer ci = do
  ident <- peek
  dbQuery1 $(selectQuery (selectContainer 'ident) "$WHERE container.id = ${ci}")

lookupVolumeContainer :: MonadDB c m => Volume -> Id Container -> m (Maybe Container)
lookupVolumeContainer vol ci =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.id = ${ci} AND container.volume = ${volumeId $ volumeRow vol}")

lookupVolumeContainers :: MonadDB c m => Volume -> m [Container]
lookupVolumeContainers vol =
  dbQuery $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.volume = ${volumeId $ volumeRow vol} ORDER BY container.id")

lookupVolumeTopContainer :: MonadDB c m => Volume -> m Container
lookupVolumeTopContainer vol =
  dbQuery1' $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.volume = ${volumeId $ volumeRow vol} ORDER BY container.id LIMIT 1")

containerIsVolumeTop :: MonadDB c m => Container -> m Bool
containerIsVolumeTop Container{ containerRow = ContainerRow{ containerTop = False } } = return False
containerIsVolumeTop c = not <$>
  dbExecute1 [pgSQL|SELECT FROM container WHERE volume = ${volumeId $ volumeRow $ containerVolume c} AND id < ${containerId $ containerRow c} LIMIT 1|]

addContainer :: MonadAudit c m => Container -> m Container
addContainer bc = do
  ident <- getAuditIdentity
  dbQuery1' $(insertContainer 'ident 'bc)

changeContainer :: MonadAudit c m => Container -> m ()
changeContainer c = do
  ident <- getAuditIdentity
  dbExecute1' $(updateContainer 'ident 'c)

removeContainer :: MonadAudit c m => Container -> m Bool
removeContainer c = do
  ident <- getAuditIdentity
  top <- dbQuery1' [pgSQL|SELECT id FROM container WHERE volume = ${volumeId $ volumeRow $ containerVolume c} ORDER BY id LIMIT 1|]
  if top == containerId (containerRow c)
    then return False
    else isRight <$> dbTryJust (guard . isForeignKeyViolation) (dbExecute1 $(deleteContainer 'ident 'c))

getContainerDate :: Container -> Maybe MaskedDate
getContainerDate c =
  maskDateIf (dataPermission2 getContainerRelease getContainerVolumePermission c == PermissionNONE)
    <$> containerDate (containerRow c)

formatContainerDate :: Container -> Maybe String
formatContainerDate c = formatTime defaultTimeLocale "%Y-%m-%d" <$> getContainerDate c

containerRowJSON :: JSON.ToObject o => Bool -> ContainerRow -> JSON.Record (Id Container) o
containerRowJSON publicRestricted ContainerRow{..} = JSON.Record containerId $
     "top" JSON..=? (True <? containerTop)
  <> "name" JSON..=? if publicRestricted then (fmap maskRestrictedString containerName) else containerName

containerJSON :: JSON.ToObject o => Bool -> Container -> JSON.Record (Id Container) o
containerJSON publicRestricted c@Container{..} = containerRowJSON publicRestricted containerRow JSON..<>
     "date" JSON..=? formatContainerDate c
  <> "release" JSON..=? containerRelease
