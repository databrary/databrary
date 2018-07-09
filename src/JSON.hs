{-# LANGUAGE OverloadedStrings, FunctionalDependencies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JSON
  ( module Data.Aeson
  , module Data.Aeson.Types
  , ToObject
  -- -- , objectEncoding
  , mapObjects
  , ToNestedObject(..)
  , (.=.)
  , kvObjectOrEmpty-- , (.=?)
  , lookupAtParse-- , (.!)
  -- , (.!?)
  , Record(..)
  , foldObjectIntoRec -- , (.<>)
  , recordObject
  , recordEncoding
  , mapRecords
  , (.=:)
  , recordMap
  -- -- , eitherJSON
  , Query
  , jsonQuery
  -- -- , escapeByteString
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V
import Network.HTTP.Types (Query)
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Text as Html

newtype UnsafeEncoding = UnsafeEncoding Encoding

instance KeyValue [Pair] where
  k .= v = [k .= v]

instance KeyValue Object where
  k .= v = HM.singleton k $ toJSON v

class (Monoid o, KeyValue o) => ToObject o

instance ToObject Series
instance ToObject [Pair]
instance ToObject Object

mapObjects :: (Functor t, Foldable t) => (a -> Series) -> t a -> Encoding
mapObjects f = foldable . fmap (UnsafeEncoding . pairs . f)

class (ToObject o, ToJSON u) => ToNestedObject o u | o -> u where
  nestObject :: ToJSON v => T.Text -> ((o -> u) -> v) -> o

instance ToJSON UnsafeEncoding where
  toJSON = error "toJSON UnsafeEncoding"
  toEncoding (UnsafeEncoding e) = e

instance ToNestedObject Series UnsafeEncoding where
  nestObject k f = k .= f (UnsafeEncoding . pairs)

instance ToNestedObject [Pair] Value where
  nestObject k f = k .= f object

instance ToNestedObject Object Value where
  nestObject k f = k .= f Object

infixr 8 .=.
(.=.) :: ToNestedObject o u => T.Text -> o -> o
k .=. v = nestObject k (\f -> f v)

-- infixr 8 .=?
-- (.=?) :: (ToObject o, ToJSON v) => T.Text -> Maybe v -> o
kvObjectOrEmpty :: (ToObject o, ToJSON v) => T.Text -> Maybe v -> o
_ `kvObjectOrEmpty` Nothing = mempty
k `kvObjectOrEmpty` (Just v) = k .= v

data Record k o = Record
  { recordKey :: !k
  , _recordObject :: o
  }

-- fold object into key + object
-- infixl 5 .<>
foldObjectIntoRec :: Monoid o => Record k o -> o -> Record k o
Record key obj `foldObjectIntoRec` obj2 = Record key $ obj <> obj2

recordObject :: (ToJSON k, ToObject o) => Record k o -> o
recordObject (Record k o) = ("id" .= k) <> o

recordEncoding :: ToJSON k => Record k Series -> Encoding
recordEncoding = pairs . recordObject

mapRecords :: (Functor t, Foldable t, ToJSON k) => (a -> Record k Series) -> t a -> Encoding
mapRecords toRecord objs = mapObjects (\obj -> (recordObject . toRecord) obj) objs

infixr 8 .=:
(.=:) :: (ToJSON k, ToNestedObject o u) => T.Text -> Record k o -> o
(.=:) k = (.=.) k . recordObject

recordMap :: (ToJSON k, ToNestedObject o u) => [Record k o] -> o
recordMap = foldMap (\r -> tt (toJSON $ recordKey r) .=. recordObject r) where
  tt (String t) = t
  tt v = TL.toStrict $ TLB.toLazyText $ encodeToTextBuilder v

lookupAtParse :: FromJSON a => Array -> Int -> Parser a
a `lookupAtParse` i = maybe (fail $ "index " ++ show i ++ " out of range") parseJSON $ a V.!? i

instance ToJSON BS.ByteString where
  toJSON = String . TE.decodeUtf8 -- questionable

instance ToJSONKey BS.ByteString where
  toJSONKey = toJSONKeyText TE.decodeUtf8

instance FromJSON BS.ByteString where
  parseJSON = fmap TE.encodeUtf8 . parseJSON

instance ToJSON Html.Html where
  toJSON = toJSON . Html.renderHtml
  toEncoding = toEncoding . Html.renderHtml

jsonQuery :: Monad m => (BS.ByteString -> Maybe BS.ByteString -> m (Maybe Encoding)) -> Query -> m Series
jsonQuery _ [] =
  return mempty
jsonQuery f ((k,mVal):qryPairs) = do
  mEncoded :: Maybe Encoding <- f k mVal
  let jsonQueryRestAct = jsonQuery f qryPairs
  (maybe
     (id :: Series -> Series)
     (\encodedObj seriesRest -> (objToPair k encodedObj) <> seriesRest)
     mEncoded)
    <$> jsonQueryRestAct
  where
    objToPair :: (KeyValue kv) => BS.ByteString -> Encoding -> kv
    objToPair key encObj = (((TE.decodeLatin1 key) .=) . UnsafeEncoding) encObj
