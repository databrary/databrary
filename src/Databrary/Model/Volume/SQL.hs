{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Volume.SQL
  ( selectVolumeRow
  -- , selectPermissionVolume
  , selectVolume
  , updateVolume
  , insertVolume
  -- for expanded queries
  , setCreation
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Model.Time
import Databrary.Model.SQL.Select
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.Types

parseOwner :: T.Text -> VolumeOwner
parseOwner t = (Id $ read $ T.unpack i, T.tail n) where
  (i, n) = T.breakOn ":" t

setCreation :: VolumeRow -> Maybe Timestamp -> [VolumeOwner] -> Permission -> VolumeAccessPolicy -> Volume
setCreation r mCreate owners perm policy =
  Volume r (fromMaybe (volumeCreation blankVolume) mCreate) owners perm policy

makePermInfo :: Maybe Permission -> Maybe Bool -> (Permission, VolumeAccessPolicy)
makePermInfo mPerm mShareFull =
  let perm = fromMaybe PermissionNONE mPerm
  in (perm, volumeAccessPolicyWithDefault perm mShareFull)

makeVolume
  :: ([VolumeOwner] -> Permission -> VolumeAccessPolicy -> a)
  -> Maybe [Maybe T.Text]
  -> (Permission, VolumeAccessPolicy)
  -> a
makeVolume vol own (perm, policy) =
  vol
    (maybe [] (map (parseOwner . fromMaybe (error "NULL volume.owner"))) own)
    perm
    policy

selectVolumeRow :: Selector -- ^ @'VolumeRow'@
selectVolumeRow = selectColumns 'VolumeRow "volume" ["id", "name", "body", "alias", "doi"]

selectPermissionVolume :: Selector -- ^ @'Permission' -> 'Volume'@
selectPermissionVolume = addSelects 'setCreation -- setCreation will be waiting on [VolumeOwner] and Permission
  selectVolumeRow
  [SelectExpr "volume_creation(volume.id)"] -- XXX explicit table references (throughout)

selectVolume :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Volume'@
selectVolume i = selectJoin 'makeVolume
  [ selectPermissionVolume
  , maybeJoinOn "volume.id = volume_owners.volume" -- join in Maybe [Maybe Text] of owners
    $ selectColumn "volume_owners" "owners"
  , joinOn "volume_permission.permission >= 'PUBLIC'::permission" -- join in Maybe Permission
      (selector
        ("LATERAL \
         \  (VALUES \
         \     ( CASE WHEN ${identitySuperuser " ++ is ++ "} \
         \             THEN enum_last(NULL::permission) \
         \             ELSE volume_access_check(volume.id, ${view " ++ is ++ " :: Id Party}) END \
         \     , CASE WHEN ${identitySuperuser " ++ is ++ "} \
         \             THEN null \
         \             ELSE (select share_full \
         \                   from volume_access_view \
         \                   where volume = volume.id and party = ${view " ++ is ++ " :: Id Party} \
         \                   limit 1) END ) \
         \  ) AS volume_permission (permission, share_full)")
        -- get rid of "volume_access_check", use query directly
        (OutputJoin
           False
           'makePermInfo
           [ (SelectColumn "volume_permission" "permission")
           , (SelectColumn "volume_permission" "share_full")]))
  ]
  where is = nameRef i

volumeKeys :: String -- ^ @'Volume'@
  -> [(String, String)]
volumeKeys v =
  [ ("id", "${volumeId $ volumeRow " ++ v ++ "}") ]

volumeSets :: String -- ^ @'Volume@
  -> [(String, String)]
volumeSets v =
  [ ("name",  "${volumeName $ volumeRow "  ++ v ++ "}")
  , ("alias", "${volumeAlias $ volumeRow " ++ v ++ "}")
  , ("body",  "${volumeBody $ volumeRow "  ++ v ++ "}")
  ]

updateVolume :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ()
updateVolume ident v = auditUpdate ident "volume"
  (volumeSets vs)
  (whereEq $ volumeKeys vs)
  Nothing
  where vs = nameRef v

insertVolume
    :: TH.Name -- ^ @'AuditIdentity'
    -> TH.Name -- ^ @'Volume'@
    -> TH.ExpQ -- ^ @'Permission' -> 'Volume'@
insertVolume ident v = auditInsert ident "!volume"
  (volumeSets vs)
  (Just $ selectOutput selectPermissionVolume)
  where vs = nameRef v

