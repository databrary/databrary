{-# LANGUAGE OverloadedStrings #-}
module Data.Csv.Contrib
  ( getHeaders
  , extractColumnsInitialRows
  , extractColumnsDistinctSample
  , extractColumnDefaulting
  , extractColumn
  , decodeCsvByNameWith
  , parseCsvWithHeader
  , removeBomPrefixText
  -- for testing only
  , removeBomPrefix
  , repairDuplicateLineEndings
  , repairCarriageReturnOnly
  ) where

import qualified Data.Attoparsec.ByteString as ATTO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Search as Search
import qualified Data.Csv as Csv
import qualified Data.Csv.Parser as Csv
import qualified Data.HashMap.Strict as HMP
import qualified Data.List as L
import qualified Data.Maybe as MB
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Vector (Vector)

getHeaders :: Csv.Header -> [BS.ByteString]
getHeaders = V.toList

extractColumnsInitialRows :: Int -> Csv.Header -> Vector Csv.NamedRecord -> [(BS.ByteString, [BS.ByteString])]
extractColumnsInitialRows maxRows hdrs records =
    zip
        hdrs'
        (fmap (`extractColumnDefaulting` truncatedRecords) hdrs')
  where
    truncatedRecords = V.take maxRows records
    hdrs' :: [BS.ByteString]
    hdrs' = getHeaders hdrs

extractColumnsDistinctSample :: Int -> Csv.Header -> Vector Csv.NamedRecord -> [(BS.ByteString, [BS.ByteString])]
extractColumnsDistinctSample maxSamples hdrs records =
    zip hdrs'
        ( fmap
              ( getSample
              . (`extractColumnDefaulting` records))
              hdrs' )
  where
    getSample :: [BS.ByteString] -> [BS.ByteString]
    getSample vals =
        (take maxSamples . L.nub) vals
    hdrs' :: [BS.ByteString]
    hdrs' = getHeaders hdrs

extractColumnDefaulting :: BS.ByteString -> Vector Csv.NamedRecord -> [BS.ByteString]
extractColumnDefaulting hdr records =
   extractColumn hdr records (maybe "" id)

extractColumn :: BS.ByteString -> Vector Csv.NamedRecord -> (Maybe BS.ByteString -> a) -> [a]
extractColumn hdr records applyDefault =
   ( V.toList
   . fmap (applyDefault . HMP.lookup hdr))
   records

-- similar to decodeByName with except make parser parameter explicitly
decodeCsvByNameWith :: (Csv.NamedRecord -> Csv.Parser a) -> BS.ByteString -> Either String (Csv.Header, Vector a)
decodeCsvByNameWith parseNamedRecord' contents = do
    -- Csv.decodeByNameWith Csv.defaultDecodeOptions contents
    (hdr, rcrds) <- parseCsvWithHeader contents
    let nRcrds = fmap Csv.toNamedRecord rcrds
    let rcrdsParser = traverse parseNamedRecord' nRcrds
    vals <- Csv.runParser rcrdsParser
    pure (hdr, vals)

parseCsvWithHeader :: BS.ByteString -> Either String (Csv.Header, Vector Csv.NamedRecord)
parseCsvWithHeader contents =
    runCsvParser ATTO.parseOnly contents

runCsvParser
    :: (ATTO.Parser (Csv.Header, Vector Csv.NamedRecord) -> BS.ByteString -> Either String (Csv.Header, Vector Csv.NamedRecord))
    -> BS.ByteString
    -> Either String (Csv.Header, Vector Csv.NamedRecord)
runCsvParser parse contents = do
    (hdrs, rows) <- parse
          (Csv.csvWithHeader Csv.defaultDecodeOptions)
          ((repairCarriageReturnOnly . repairDuplicateLineEndings . removeBomPrefix) contents)
    pure (hdrs, scrub rows)

scrub :: Vector Csv.NamedRecord -> Vector Csv.NamedRecord
scrub rows =
    fmap scrubRow rows
  where
    scrubRow :: Csv.NamedRecord -> Csv.NamedRecord
    scrubRow row =
        HMP.map (\v -> if BSC.all (== ' ') v then "" else v) row

-- | some programs introduce a byte order mark when generating a CSV, remove this per cassava issue recipe
removeBomPrefix :: BS.ByteString -> BS.ByteString
removeBomPrefix contents =
    MB.fromMaybe contents (BS.stripPrefix "\357\273\277" contents)

-- | some programs introduce a byte order mark when generating a CSV, remove this per cassava issue recipe
removeBomPrefixText :: TL.Text -> TL.Text
removeBomPrefixText contents =
    case TL.uncons contents of
        Just ('\65279', rs) -> rs
        _ -> contents

-- | fix duplicate line endings, unclear if SPSS or Excel introduces them
repairDuplicateLineEndings :: BS.ByteString -> BS.ByteString
repairDuplicateLineEndings contents =
    BSL.toStrict (Search.replace "\r\r\n" ("\r\n" :: BS.ByteString) contents)

-- | only fix newlines for bizarre macOS endings that use \r instead of \r\n
repairCarriageReturnOnly :: BS.ByteString -> BS.ByteString
repairCarriageReturnOnly contents =
    let
        hasNewline = BSC.elem '\n' contents
    in
        if hasNewline
        then contents
        else BSC.concatMap (\c -> if c == '\r' then "\r\n" else BSC.singleton c) contents
