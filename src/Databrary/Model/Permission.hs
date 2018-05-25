{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission
  ( module Databrary.Model.Permission.Types
  , permissionVIEW
  , permissionPRIVATE
  , readPermission
  , readRelease
  -- , dataPermission3
  , dataPermission4
  -- , canReadData
  , canReadData2
  , accessJSON
  -- testing only
  , testdataPermission3
  ) where

import Data.Monoid ((<>))

import qualified Databrary.JSON as JSON
import Databrary.Model.Release.Types
import Databrary.Model.Permission.Types

-- |Level at which things become visible.
permissionVIEW :: Permission
permissionVIEW = PermissionPUBLIC

-- |Alias for READ. Grants full access to private data, bypassing consent permissions.
permissionPRIVATE :: Permission
permissionPRIVATE = PermissionREAD

-- |The necessary permission level to read a data object with the given release.
-- Equivalent to the SQL function read_permission.
readPermission :: Release -> Permission
readPermission ReleasePUBLIC   = PermissionPUBLIC
readPermission ReleaseSHARED   = PermissionSHARED
readPermission ReleaseEXCERPTS = PermissionSHARED
readPermission ReleasePRIVATE  = permissionPRIVATE

-- |The most restrictive data release level that the current user may access under the given permission.
-- Equivalent to the SQL function read_release.  Inverse of 'readPermission' module meaning of @Nothing@.
readRelease :: Permission -> Maybe Release
readRelease PermissionNONE   = Nothing
readRelease PermissionPUBLIC = Just ReleasePUBLIC
readRelease PermissionSHARED = Just ReleaseSHARED
readRelease _                = Just ReleasePRIVATE

-- |The effective permission for data objects with the given attributes, effectively collapsing ineffective permissions NONE.
releasePermission :: Release -> Permission -> Permission
releasePermission effectiveReleaseOnData currentUserAllowedPermissionOnVolume
  | currentUserAllowedPermissionOnVolume >= readPermission effectiveReleaseOnData = currentUserAllowedPermissionOnVolume
  | otherwise = PermissionNONE

dataPermission4 :: (a -> EffectiveRelease) -> (a -> VolumeRolePolicy) -> a -> Permission
dataPermission4 getObjEffectiveRelease getCurrentUserVolumeRole obj =
  let
   effRelease = getObjEffectiveRelease obj
  in 
    case getCurrentUserVolumeRole obj of
      RolePublicViewer PublicRestrictedPolicy ->
        releasePermission (effRelPrivate effRelease) PermissionPUBLIC
      RoleSharedViewer SharedRestrictedPolicy ->
        releasePermission (effRelPrivate effRelease) PermissionSHARED
      -- other levels that behave more like private (options: none, shared, read, edit, admin) ?
      rp ->
         releasePermission (effRelPublic effRelease) (extractPermissionIgnorePolicy rp)

canReadData2 :: (a -> EffectiveRelease) -> (a -> VolumeRolePolicy) -> a -> Bool
canReadData2 getObjEffectiveRelease getCurrentUserVolumeRole obj =
  dataPermission4 getObjEffectiveRelease getCurrentUserVolumeRole obj > PermissionNONE

accessJSON :: JSON.ToObject o => Access -> o
accessJSON Access{..} =
     "site" JSON..= accessSite'
  <> "member" JSON..= accessMember'
