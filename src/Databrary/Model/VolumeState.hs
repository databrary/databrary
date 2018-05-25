{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.VolumeState
  ( module Databrary.Model.VolumeState.Types
  , lookupVolumeState
  , changeVolumeState
  , removeVolumeState
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.VolumeState.Types
import Databrary.Model.VolumeState.SQL

lookupVolumeState :: (MonadDB c m) => Volume -> m [VolumeState]
lookupVolumeState v =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeState "$WHERE volume = ${volumeId $ volumeRow v} AND (public OR ${(extractPermissionIgnorePolicy . volumeRolePolicy) v >= PermissionEDIT})")

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

changeVolumeState :: (MonadDB c m) => VolumeState -> m ()
changeVolumeState VolumeState{..} = do
  let _tenv_a5HPz = unknownPGTypeEnv
      _tenv_a5FNg = unknownPGTypeEnv
  void $ updateOrInsert
    -- [pgSQL|UPDATE volume_state SET value = ${volumeStateValue}, public = ${volumeStatePublic} WHERE volume = ${volumeId $ volumeRow stateVolume} AND key = ${volumeStateKey}|]
    (mapQuery
        ((\ _p_a5FNh _p_a5FNi _p_a5FNj _p_a5FNk ->
                    (Data.ByteString.concat
                       [Data.String.fromString "UPDATE volume_state SET value = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "jsonb")
                          _p_a5FNh,
                        Data.String.fromString ", public = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                          _p_a5FNi,
                        Data.String.fromString " WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a5FNj,
                        Data.String.fromString " AND key = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a5FNk]))
         volumeStateValue
         volumeStatePublic
         (volumeId $ volumeRow stateVolume)
         volumeStateKey)
        (\[] -> ()))
    -- [pgSQL|INSERT INTO volume_state (volume, key, value, public) VALUES (${volumeId $ volumeRow stateVolume}, ${volumeStateKey}, ${volumeStateValue}, ${volumeStatePublic})|]
    (mapQuery
       ((\ _p_a5HPA _p_a5HPB _p_a5HPC _p_a5HPD ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "INSERT INTO volume_state (volume, key, value, public) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a5HPA,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a5HPB,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "jsonb")
                          _p_a5HPC,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                          _p_a5HPD,
                        Data.String.fromString ")"]))
         (volumeId $ volumeRow stateVolume)
         volumeStateKey
         volumeStateValue
         volumeStatePublic)
       (\[] -> ()))


removeVolumeState :: (MonadDB c m) => Volume -> VolumeStateKey -> m Bool
removeVolumeState v k = do
  let _tenv_a5N5m = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|DELETE FROM volume_state WHERE volume = ${volumeId $ volumeRow v} AND key = ${k}|]
   (mapQuery
     ((\ _p_a5N5n _p_a5N5o ->
                    (Data.ByteString.concat
                       [Data.String.fromString "DELETE FROM volume_state WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5N5m
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a5N5n,
                        Data.String.fromString " AND key = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5N5m
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a5N5o]))
      (volumeId $ volumeRow v) k)
     (\[] -> ()))


