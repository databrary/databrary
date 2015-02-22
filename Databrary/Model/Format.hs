{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Format
  ( module Databrary.Model.Format.Types
  , unknownFormat
  , allFormats
  , getFormat
  , getFormat'
  , getFormatByFilename
  , formatJSON
  ) where

import Data.Char (toLower)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import System.FilePath (takeExtension)

import Control.Applicative.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Format.Types
import Databrary.Model.Format.Boot

unknownFormat :: Format
unknownFormat = Format
  { formatId = error "unknown format"
  , formatMimeType = "application/octet-stream"
  , formatExtension = []
  , formatName = "Unknown"
  }

allFormats :: [Format]
allFormats = $(loadFormats)

formatsById :: IntMap.IntMap Format
formatsById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ formatId a, a)) allFormats

getFormat :: Id Format -> Maybe Format
getFormat (Id i) = IntMap.lookup (fromIntegral i) formatsById

getFormat' :: Id Format -> Format
getFormat' (Id i) = formatsById IntMap.! fromIntegral i

formatsByExtension :: Map.Map String Format
formatsByExtension = Map.fromList [ (e, a) | a <- allFormats, e <- formatExtension a ]

getFormatByExtension :: String -> Maybe Format
getFormatByExtension e = Map.lookup (map toLower e) formatsByExtension

getFormatByFilename :: FilePath -> Maybe Format
getFormatByFilename = me . takeExtension where
  me ('.':e) = getFormatByExtension e
  me _ = Nothing

formatJSON :: Format -> JSON.Object
formatJSON Format{..} = JSON.record formatId $ catMaybes
  [ Just $ "mimetype" JSON..= formatMimeType
  , null formatExtension ?!> "extension" JSON..= formatExtension
  , Just $ "name" JSON..= formatName
  -- TODO: description, transcodable
  ]
