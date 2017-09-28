{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, DataKinds #-}
module Databrary.Model.Tag
  ( module Databrary.Model.Tag.Types
  , lookupTag
  , lookupTags
  , findTags
  , addTag
  , lookupVolumeTagUseRows
  , addTagUse
  , removeTagUse
  , lookupTopTagWeight
  , lookupTagCoverage
  , lookupSlotTagCoverage
  , lookupSlotKeywords
  , tagWeightJSON
  , tagCoverageJSON
  ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (parseQueryFlags)

import Databrary.Ops
import Databrary.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.SQL.Select
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL

lookupTag :: MonadDB c m => TagName -> m (Maybe Tag)
lookupTag n =
  dbQuery1 
      $(makeQuery
          (fst (parseQueryFlags "$WHERE tag.name = ${n}::varchar"))
          (\_ -> 
                 "SELECT tag.id,tag.name"
              ++ " FROM tag " 
              ++ (snd (parseQueryFlags "$WHERE tag.name = ${n}::varchar")))
          (OutputJoin
             False 
             'Tag 
             [ SelectColumn "tag" "id"
             , SelectColumn "tag" "name" ]))

lookupTags :: MonadDB c m => m [Tag]
lookupTags = 
  dbQuery
      $(makeQuery
          (fst (parseQueryFlags ""))
          (\_ -> 
                 "SELECT tag.id,tag.name"
              ++ " FROM tag " 
              ++ (snd (parseQueryFlags "")))
          (OutputJoin
             False 
             'Tag 
             [ SelectColumn "tag" "id"
             , SelectColumn "tag" "name" ]))

findTags :: MonadDB c m => TagName -> Int -> m [Tag]
findTags (TagName n) lim = -- TagName restrictions obviate pattern escaping
  dbQuery
      $(makeQuery
          (fst (parseQueryFlags "$WHERE tag.name LIKE ${n `BSC.snoc` '%'}::varchar LIMIT ${fromIntegral lim :: Int64}"))
          (\_ -> 
                 "SELECT tag.id,tag.name"
              ++ " FROM tag " 
              ++ (snd (parseQueryFlags "$WHERE tag.name LIKE ${n `BSC.snoc` '%'}::varchar LIMIT ${fromIntegral lim :: Int64}")))
          (OutputJoin
             False 
             'Tag 
             [ SelectColumn "tag" "id"
             , SelectColumn "tag" "name" ]))

addTag :: MonadDB c m => TagName -> m Tag
addTag n =
  dbQuery1' $ (`Tag` n) <$> [pgSQL|!SELECT get_tag(${n})|]

lookupVolumeTagUseRows :: MonadDB c m => Volume -> m [TagUseRow]
lookupVolumeTagUseRows v =
  dbQuery $(selectQuery selectTagUseRow "JOIN container ON tag_use.container = container.id WHERE container.volume = ${volumeId $ volumeRow v} ORDER BY container.id")

addTagUse :: MonadDB c m => TagUse -> m Bool
addTagUse t = either (const False) id <$> do
  dbTryJust (guard . isExclusionViolation)
    $ dbExecute1 (if tagKeyword t
      then $(insertTagUse True 't)
      else $(insertTagUse False 't))

removeTagUse :: MonadDB c m => TagUse -> m Int
removeTagUse t =
  dbExecute
    (if tagKeyword t
      then $(deleteTagUse True 't)
      else $(deleteTagUse False 't))

lookupTopTagWeight :: MonadDB c m => Int -> m [TagWeight]
lookupTopTagWeight lim =
  dbQuery $(selectQuery (selectTagWeight "") "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

emptyTagCoverage :: Tag -> Container -> TagCoverage
emptyTagCoverage t c = TagCoverage (TagWeight t 0) c [] [] []

lookupTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Tag -> Slot -> m TagCoverage
lookupTagCoverage t (Slot c s) = do
  ident <- peek
  fromMaybe (emptyTagCoverage t c) <$> dbQuery1 (($ c) . ($ t) <$> $(selectQuery (selectTagCoverage 'ident "WHERE container = ${containerId $ containerRow c} AND segment && ${s} AND tag = ${tagId t}") "$!"))

lookupSlotTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Slot -> Int -> m [TagCoverage]
lookupSlotTagCoverage slot lim = do
  ident <- peek
  dbQuery $(selectQuery (selectSlotTagCoverage 'ident 'slot) "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

lookupSlotKeywords :: (MonadDB c m) => Slot -> m [Tag]
lookupSlotKeywords Slot{..} =
  dbQuery
      $(makeQuery
          (fst (parseQueryFlags "JOIN keyword_use ON id = tag WHERE container = ${containerId $ containerRow slotContainer} AND segment = ${slotSegment}"))
          (\_ -> 
                 "SELECT tag.id,tag.name"
              ++ " FROM tag " 
              ++ (snd (parseQueryFlags "JOIN keyword_use ON id = tag WHERE container = ${containerId $ containerRow slotContainer} AND segment = ${slotSegment}")))
          (OutputJoin
             False 
             'Tag 
             [ SelectColumn "tag" "id"
             , SelectColumn "tag" "name" ]))


tagWeightJSON :: JSON.ToObject o => TagWeight -> JSON.Record TagName o
tagWeightJSON TagWeight{..} = JSON.Record (tagName tagWeightTag) $
  "weight" JSON..= tagWeightWeight

tagCoverageJSON :: JSON.ToObject o => TagCoverage -> JSON.Record TagName o
tagCoverageJSON TagCoverage{..} = tagWeightJSON tagCoverageWeight JSON..<>
     "coverage" JSON..= tagCoverageSegments
  <> "keyword" JSON..=? (tagCoverageKeywords <!? null tagCoverageKeywords)
  <> "vote"    JSON..=? (tagCoverageVotes    <!? null tagCoverageVotes)
