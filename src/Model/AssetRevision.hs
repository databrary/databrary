{-# LANGUAGE DataKinds #-}
module Model.AssetRevision
  ( module Model.AssetRevision.Types
  , replaceAsset
  , assetIsReplaced
  , lookupAssetReplace
  , lookupAssetTranscode
  ) where

import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Has
import Service.DB
import Model.Id
import Model.Party
import Model.Identity
import Model.Asset
import Model.Asset.SQL
import Model.AssetRevision.Types
import Model.Volume.SQL
import Model.Volume.Types

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

replaceAsset :: MonadDB c m => Asset -> Asset -> m ()
replaceAsset old new = do
  let _tenv_a8Fao = unknownPGTypeEnv
  dbExecute1' -- [pgSQL|SELECT asset_replace(${assetId $ assetRow old}, ${assetId $ assetRow new})|]
   (mapQuery
     ((\ _p_a8Fap _p_a8Faq ->
                    (Data.ByteString.concat
                       [Data.String.fromString "SELECT asset_replace(",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Fao
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8Fap,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Fao
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8Faq,
                        Data.String.fromString ")"]))
      (assetId $ assetRow old) (assetId $ assetRow new))
            (\[_casset_replace_a8Far]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a8Fao
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "void")
                     _casset_replace_a8Far)))


assetIsReplaced :: MonadDB c m => Asset -> m Bool
assetIsReplaced a = do
  let _tenv_a8FgX = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|SELECT ''::void FROM asset_replace WHERE orig = ${assetId $ assetRow a} LIMIT 1|]
    (mapQuery
      ((\ _p_a8FgY ->
                    Data.ByteString.concat
                       [Data.String.fromString
                          "SELECT ''::void FROM asset_replace WHERE orig = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8FgX
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8FgY,
                        Data.String.fromString " LIMIT 1"])
       (assetId $ assetRow a))
            (\[_cvoid_a8FgZ]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a8FgX
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "void")
                     _cvoid_a8FgZ)))


lookupAssetReplace :: (MonadHasIdentity c m, MonadDB c m) => Asset -> m (Maybe AssetRevision)
lookupAssetReplace a = do
  let _tenv_abkQ9 = unknownPGTypeEnv
  ident <- peek
  mRow <- -- dbQuery1 ($ a) <$> $(selectQuery (selectAssetRevision "asset_replace" 'ident) "$WHERE asset_replace.asset = ${assetId $ assetRow a}")
   mapRunPrepQuery1
      ((\ _p_abkQa _p_abkQb _p_abkQc _p_abkQd _p_abkQe ->
                       (Data.String.fromString
                          "SELECT asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission,volume_permission.share_full FROM asset_replace JOIN asset JOIN volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL   (VALUES      ( CASE WHEN $1              THEN enum_last(NULL::permission)              ELSE volume_access_check(volume.id, $2) END      , CASE WHEN $3              THEN null              ELSE (select share_full                    from volume_access_view                    where volume = volume.id and party = $4                    limit 1) END )   ) AS volume_permission (permission, share_full) ON volume_permission.permission >= 'PUBLIC'::permission ON asset.volume = volume.id ON asset_replace.orig = asset.id WHERE asset_replace.asset = $5",
                       [pgEncodeParameter
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "boolean") _p_abkQa,
                        pgEncodeParameter
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "integer") _p_abkQb,
                        pgEncodeParameter
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "boolean") _p_abkQc,
                        pgEncodeParameter
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "integer") _p_abkQd,
                        pgEncodeParameter
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "integer") _p_abkQe],
                       [pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "smallint"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "release"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "interval"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "bytea"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "bigint"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "timestamp with time zone"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text[]"),
                        pgBinaryColumn
                          _tenv_abkQ9 (PGTypeProxy :: PGTypeName "permission"),
                        pgBinaryColumn _tenv_abkQ9 (PGTypeProxy :: PGTypeName "boolean")]))
         (identitySuperuser ident)
         (view ident :: Id Party)
         (identitySuperuser ident)
         (view ident :: Id Party)
         (assetId $ assetRow a))
               (\
                  [_cid_abkQf,
                   _cformat_abkQg,
                   _crelease_abkQh,
                   _cduration_abkQi,
                   _cname_abkQj,
                   _csha1_abkQk,
                   _csize_abkQl,
                   _cid_abkQm,
                   _cname_abkQn,
                   _cbody_abkQo,
                   _calias_abkQp,
                   _cdoi_abkQq,
                   _cvolume_creation_abkQr,
                   _cowners_abkQs,
                   _cpermission_abkQt,
                   _cshare_full_abkQu]
                  -> (pgDecodeColumnNotNull
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "integer") _cid_abkQf,
                      pgDecodeColumnNotNull
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "smallint") _cformat_abkQg,
                      pgDecodeColumn
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "release") _crelease_abkQh,
                      pgDecodeColumn
                        _tenv_abkQ9
                        (PGTypeProxy :: PGTypeName "interval")
                        _cduration_abkQi,
                      pgDecodeColumn
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text") _cname_abkQj,
                      pgDecodeColumn
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "bytea") _csha1_abkQk,
                      pgDecodeColumn
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "bigint") _csize_abkQl,
                      pgDecodeColumnNotNull
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "integer") _cid_abkQm,
                      pgDecodeColumnNotNull
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text") _cname_abkQn,
                      pgDecodeColumn
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text") _cbody_abkQo,
                      pgDecodeColumn
                        _tenv_abkQ9
                        (PGTypeProxy :: PGTypeName "character varying")
                        _calias_abkQp,
                      pgDecodeColumn
                        _tenv_abkQ9
                        (PGTypeProxy :: PGTypeName "character varying")
                        _cdoi_abkQq,
                      pgDecodeColumn
                        _tenv_abkQ9
                        (PGTypeProxy :: PGTypeName "timestamp with time zone")
                        _cvolume_creation_abkQr,
                      pgDecodeColumnNotNull
                        _tenv_abkQ9 (PGTypeProxy :: PGTypeName "text[]") _cowners_abkQs,
                      pgDecodeColumn
                        _tenv_abkQ9
                        (PGTypeProxy :: PGTypeName "permission")
                        _cpermission_abkQt,
                      pgDecodeColumn
                        _tenv_abkQ9
                        (PGTypeProxy :: PGTypeName "boolean")
                        _cshare_full_abkQu))
  pure
    (fmap
      (\ (vid_abkPn, vformat_abkPo, vrelease_abkPp, vduration_abkPq,
          vname_abkPr, vc_abkPs, vsize_abkPt, vid_abkPu, vname_abkPv,
          vbody_abkPw, valias_abkPx, vdoi_abkPy, vc_abkPz, vowners_abkPA,
          vpermission_abkPB, vfull_abkPC)
         -> AssetRevision
              (Asset
                 (Model.Asset.SQL.makeAssetRow
                    vid_abkPn
                    vformat_abkPo
                    vrelease_abkPp
                    vduration_abkPq
                    vname_abkPr
                    vc_abkPs
                    vsize_abkPt)
                 (Model.Volume.SQL.makeVolume
                    (Model.Volume.SQL.setCreation
                       (Model.Volume.Types.VolumeRow
                          vid_abkPu vname_abkPv vbody_abkPw valias_abkPx vdoi_abkPy)
                       vc_abkPz)
                    vowners_abkPA
                    (Model.Volume.SQL.makePermInfo
                       vpermission_abkPB vfull_abkPC)))
              a)
      mRow)

lookupAssetTranscode :: (MonadHasIdentity c m, MonadDB c m) => Asset -> m (Maybe AssetRevision)
lookupAssetTranscode a = do
  let _tenv_abkVg = unknownPGTypeEnv
  ident <- peek
  -- dbQuery1 $ ($ a) <$> $(selectQuery (selectAssetRevision "transcode" 'ident) "$WHERE transcode.asset = ${assetId $ assetRow a}")
  mRow <- mapRunPrepQuery1
      ((\ _p_abkVh _p_abkVi _p_abkVj _p_abkVk _p_abkVl ->
                       (Data.String.fromString
                          "SELECT asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission,volume_permission.share_full FROM transcode JOIN asset JOIN volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL   (VALUES      ( CASE WHEN $1              THEN enum_last(NULL::permission)              ELSE volume_access_check(volume.id, $2) END      , CASE WHEN $3              THEN null              ELSE (select share_full                    from volume_access_view                    where volume = volume.id and party = $4                    limit 1) END )   ) AS volume_permission (permission, share_full) ON volume_permission.permission >= 'PUBLIC'::permission ON asset.volume = volume.id ON transcode.orig = asset.id WHERE transcode.asset = $5",
                       [pgEncodeParameter
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "boolean") _p_abkVh,
                        pgEncodeParameter
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "integer") _p_abkVi,
                        pgEncodeParameter
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "boolean") _p_abkVj,
                        pgEncodeParameter
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "integer") _p_abkVk,
                        pgEncodeParameter
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "integer") _p_abkVl],
                       [pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "smallint"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "release"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "interval"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "bytea"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "bigint"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "timestamp with time zone"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "text[]"),
                        pgBinaryColumn
                          _tenv_abkVg (PGTypeProxy :: PGTypeName "permission"),
                        pgBinaryColumn _tenv_abkVg (PGTypeProxy :: PGTypeName "boolean")]))
         (identitySuperuser ident)
         (view ident :: Id Party)
         (identitySuperuser ident)
         (view ident :: Id Party)
         (assetId $ assetRow a))
               (\
                  [_cid_abkVm,
                   _cformat_abkVn,
                   _crelease_abkVo,
                   _cduration_abkVp,
                   _cname_abkVq,
                   _csha1_abkVr,
                   _csize_abkVs,
                   _cid_abkVt,
                   _cname_abkVu,
                   _cbody_abkVv,
                   _calias_abkVw,
                   _cdoi_abkVx,
                   _cvolume_creation_abkVy,
                   _cowners_abkVz,
                   _cpermission_abkVA,
                   _cshare_full_abkVB]
                  -> (pgDecodeColumnNotNull
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "integer") _cid_abkVm,
                      pgDecodeColumnNotNull
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "smallint") _cformat_abkVn,
                      pgDecodeColumn
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "release") _crelease_abkVo,
                      pgDecodeColumn
                        _tenv_abkVg
                        (PGTypeProxy :: PGTypeName "interval")
                        _cduration_abkVp,
                      pgDecodeColumn
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "text") _cname_abkVq,
                      pgDecodeColumn
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "bytea") _csha1_abkVr,
                      pgDecodeColumn
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "bigint") _csize_abkVs,
                      pgDecodeColumnNotNull
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "integer") _cid_abkVt,
                      pgDecodeColumnNotNull
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "text") _cname_abkVu,
                      pgDecodeColumn
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "text") _cbody_abkVv,
                      pgDecodeColumn
                        _tenv_abkVg
                        (PGTypeProxy :: PGTypeName "character varying")
                        _calias_abkVw,
                      pgDecodeColumn
                        _tenv_abkVg
                        (PGTypeProxy :: PGTypeName "character varying")
                        _cdoi_abkVx,
                      pgDecodeColumn
                        _tenv_abkVg
                        (PGTypeProxy :: PGTypeName "timestamp with time zone")
                        _cvolume_creation_abkVy,
                      pgDecodeColumnNotNull
                        _tenv_abkVg (PGTypeProxy :: PGTypeName "text[]") _cowners_abkVz,
                      pgDecodeColumn
                        _tenv_abkVg
                        (PGTypeProxy :: PGTypeName "permission")
                        _cpermission_abkVA,
                      pgDecodeColumn
                        _tenv_abkVg
                        (PGTypeProxy :: PGTypeName "boolean")
                        _cshare_full_abkVB))
  pure
    (fmap
      (\ (vid_abkSc, vformat_abkSd, vrelease_abkSe, vduration_abkSf,
          vname_abkSg, vc_abkSh, vsize_abkSi, vid_abkSj, vname_abkSk,
          vbody_abkSl, valias_abkSm, vdoi_abkSn, vc_abkSo, vowners_abkSp,
          vpermission_abkSq, vfull_abkSr)
         -> AssetRevision
              (Asset
                 (Model.Asset.SQL.makeAssetRow
                    vid_abkSc
                    vformat_abkSd
                    vrelease_abkSe
                    vduration_abkSf
                    vname_abkSg
                    vc_abkSh
                    vsize_abkSi)
                 (Model.Volume.SQL.makeVolume
                    (Model.Volume.SQL.setCreation
                       (Model.Volume.Types.VolumeRow
                          vid_abkSj vname_abkSk vbody_abkSl valias_abkSm vdoi_abkSn)
                       vc_abkSo)
                    vowners_abkSp
                    (Model.Volume.SQL.makePermInfo
                       vpermission_abkSq vfull_abkSr)))
              a)
      mRow)

