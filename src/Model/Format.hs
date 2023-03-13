{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Model.Format
  ( module Model.Format.Types
  , mimeTypeTop
  , mimeTypeSub
  , mimeTypeTopCompare
  -- , unknownFormat
  , allFormats
  , getFormat
  , getFormat'
  , getFormatByExtension
  , addFormatExtension
  , getFormatByFilename
  , dropFormatExtension
  , videoFormat
  , imageFormat
  , audioFormat
  , formatIsImage
  , formatTranscodable
  , formatSample
  , formatJSON
  , formatIsAV
  , formatNotAV
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import System.Posix.FilePath (RawFilePath, splitExtension, takeExtension, addExtension)

import qualified JSON
import Model.Id
import Model.Format.Types

-- | Parse a mimetype string into its type (category) and subtype (specific type)
mimeTypes :: BS.ByteString -> (BS.ByteString, BS.ByteString)
mimeTypes s = maybe (s, "") (\i -> (BS.take i s, BS.drop (succ i) s)) $ BSC.elemIndex '/' s

-- | Parse a full mimetype value into just its category or type
mimeTypeTop :: BS.ByteString -> BS.ByteString
mimeTypeTop = fst . mimeTypes

-- | Parse a full mimetype value into just its subtype
mimeTypeSub :: BS.ByteString -> BS.ByteString
mimeTypeSub = snd . mimeTypes

-- | Establish a relative order between to specific mimetype values ...
mimeTypeTopCompare :: BS.ByteString -> BS.ByteString -> Ordering
mimeTypeTopCompare a b = mttc (BSC.unpack a) (BSC.unpack b) where
  mttc []      []      = EQ
  mttc ('/':_) []      = EQ
  mttc []      ('/':_) = EQ
  mttc ('/':_) ('/':_) = EQ
  mttc ('/':_) _       = LT
  mttc []      _       = LT
  mttc _       ('/':_) = GT
  mttc _       []      = GT
  mttc (ac:as) (bc:bs) = compare ac bc <> mttc as bs

{- formerly used when reading format from db
makeFormat :: Id Format -> BS.ByteString -> [Maybe BS.ByteString] -> T.Text -> Format
makeFormat i m e n = Format i m (map (fromMaybe (error "NULL format.extension")) e) n

formatRow :: Selector -- Format
formatRow = selectColumns 'makeFormat "format" ["id", "mimetype", "extension", "name"]
-}

-- | Hardcoded list of all formats recognized by Databrary for uploading
-- TODO: db coherence
allFormats :: [Format]
allFormats
    = [ Format (Id (-800)) "video/mp4"  ["mp4"]         "MPEG-4 video"
      , Format (Id (-700)) "image/jpeg" ["jpg", "jpeg"] "JPEG image"
      , Format (Id (-600))
               "audio/mpeg"
               ["mp3"]
               "MPEG-1 or MPEG-2 audio layer III"
      , Format (Id 1) "text/plain"         ["txt"] "Plain text"
      , Format (Id 2) "text/csv"           ["csv"] "Comma-separated values"
      , Format (Id 4) "text/rtf"           ["rtf"] "Rich text format"
      , Format (Id 5) "image/png"          ["png"] "Portable network graphics"
      , Format (Id 6) "application/pdf"    ["pdf"] "Portable document"
      , Format (Id 7) "application/msword" ["doc"] "Microsoft Word document"
      , Format (Id 8)
               "application/vnd.oasis.opendocument.text"
               ["odf"]
               "OpenDocument text"
      , Format
          (Id 9)
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
          ["docx"]
          "Microsoft Word (Office Open XML) document"
      , Format (Id 10)
               "application/vnd.ms-excel"
               ["xls"]
               "Microsoft Excel spreadsheet"
      , Format (Id 11)
               "application/vnd.oasis.opendocument.spreadsheet"
               ["ods"]
               "OpenDocument spreadsheet"
      , Format
          (Id 12)
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          ["xlsx"]
          "Microsoft Excel (Office Open XML) workbook"
      , Format (Id 13)
               "application/vnd.ms-powerpoint"
               ["ppt"]
               "Microsoft PowerPoint presentation"
      , Format (Id 14)
               "application/vnd.oasis.opendocument.presentation"
               ["odp"]
               "OpenDocument presentation"
      , Format
          (Id 15)
          "application/vnd.openxmlformats-officedocument.presentationml.presentation"
          ["pptx"]
          "Microsoft PowerPoint (Office Open XML) presentation"
      , Format (Id 16) "application/vnd.datavyu" ["opf"]  "Datavyu"
      , Format (Id 18) "video/webm"              ["webm"] "WebM video"
      , Format (Id 19)
               "video/mpeg"
               ["mpg", "mpeg"]
               "MPEG program stream (MPEG-1/MPEG-2 video)"
      , Format (Id 20) "video/quicktime" ["mov"] "QuickTime video"
      , Format (Id 21) "video/mp2t" ["mts", "m2ts"] "MPEG transport stream"
      , Format (Id 22) "video/avi" ["avi"] "Audio Video Interleave"
      , Format (Id 23) "application/x-spss-sav" ["sav"] "SPSS System File"
      , Format (Id 24) "audio/wav" ["wav"] "Waveform audio"
      , Format (Id 25) "video/x-ms-wmv" ["wmv"] "Windows Media video"
      , Format (Id 26)
               "text/x-chat"
               ["cha", "chat"]
               "Codes for the Human Analysis of Transcripts"
      , Format (Id 27) "audio/aac"      ["aac"] "Advanced Audio Coding"
      , Format (Id 28) "audio/x-ms-wma" ["wma"] "Windows Media audio"
      , Format (Id 29)
               "application/vnd.lena.interpreted-time-segments"
               ["its"]
               "LENA Interpreted Time Segments"
      , Format (Id 30)
               "video/x-dv"
               ["dv", "dif"]
               "Digital Interface Format video"
      , Format (Id 31)
               "text/elan"
               ["eaf", "pfsx", "etf"]
               "ELAN - Linguistic Annotator"
      ]

formatsById :: IntMap.IntMap Format
formatsById = IntMap.fromList $ map (\a -> (fromIntegral $ unId $ formatId a, a)) allFormats

getFormat :: Id Format -> Maybe Format
getFormat (Id i) = IntMap.lookup (fromIntegral i) formatsById

getFormat' :: Id Format -> Format
getFormat' (Id i) = formatsById IntMap.! fromIntegral i

formatsByExtension :: Map.Map BS.ByteString Format
formatsByExtension = Map.fromList [ (e, a) | a <- allFormats, e <- formatExtension a ]

getFormatByExtension :: BS.ByteString -> Maybe Format
getFormatByExtension e = Map.lookup (BSC.map toLower e) formatsByExtension

addFormatExtension :: RawFilePath -> Format -> RawFilePath
addFormatExtension p (formatExtension -> (e:_)) = addExtension p e
addFormatExtension p _ = p

getFormatByFilename :: RawFilePath -> Maybe Format
getFormatByFilename n = do
  ('.',e) <- BSC.uncons $ takeExtension n
  getFormatByExtension e

dropFormatExtension :: Format -> RawFilePath -> RawFilePath
dropFormatExtension fmt n
  | (f,BSC.uncons -> Just ('.',e)) <- splitExtension n
  , BSC.map toLower e `elem` formatExtension fmt = f
  | otherwise = n

-- | Blessed video format, used as output from transcoding, as well as understood by front end for playback
videoFormat :: Format
videoFormat = getFormat' (Id (-800))

imageFormat :: Format
imageFormat = getFormat' (Id (-700))

-- | Blessed audio format, used as output from transcoding, as well as understood by front end for playback
audioFormat :: Format
audioFormat = getFormat' (Id (-600))

-- | Is this a video format
formatIsVideo :: Format -> Bool
formatIsVideo Format{ formatMimeType = t } = "video/" `BS.isPrefixOf` t

-- | Is this an image format
formatIsImage :: Format -> Bool
formatIsImage Format{ formatMimeType = t } = "image/" `BS.isPrefixOf` t

-- | Is this an audio format
formatIsAudio :: Format -> Bool
formatIsAudio Format{ formatMimeType = t } = "audio/" `BS.isPrefixOf` t

-- | Is this an Audio or Video format
formatIsAV :: Format -> Bool
formatIsAV fmat = formatIsVideo fmat || formatIsAudio fmat

formatNotAV :: Format -> Bool
formatNotAV fmat = not (formatIsVideo fmat || formatIsAudio fmat)

-- | If the format can (or should be) transcoded into the blessed internal format,
-- then provide the format it will be transcoded into
formatTranscodable :: Format -> Maybe Format
formatTranscodable f
  | formatIsVideo f = Just videoFormat
  | formatIsAudio f = Just audioFormat
  | otherwise = Nothing

-- | For formats where we can produce samples, determine the type for sample output
formatSample :: Format -> Maybe Format
formatSample f
  | f == videoFormat = Just imageFormat
  | otherwise = Nothing

-- | Convert a Format value into the shape of JSON to be produced.
-- Arbitrarily only show the first extension if multiple extensions are present
formatJSON :: JSON.ToObject o => Format -> JSON.Record (Id Format) o
formatJSON f = JSON.Record (formatId f) $
     "mimetype" JSON..= formatMimeType f
  <> "extension" `JSON.kvObjectOrEmpty` listToMaybe (formatExtension f)
  <> "name" JSON..= formatName f
  <> "transcodable" `JSON.kvObjectOrEmpty` (formatId <$> formatTranscodable f)
  -- TODO: description
