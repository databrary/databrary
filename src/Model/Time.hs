{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Time
  ( Date
  , Timestamp
  , dateYear
  , MaskedDate
  , maskDateIf
  , maskedYear
  ) where

import qualified Data.Aeson as JSON
import Data.Fixed (Fixed(..))
import Data.Time (Day(..), UTCTime(..), DiffTime, toGregorian, fromGregorian)
import Data.Time.Format (FormatTime(..), formatTime, dateFmt)
import Language.Haskell.TH.Lift (deriveLiftMany)

-- | Synonym for a Day
type Date = Day
-- | Synonym for a UTCTime
type Timestamp = UTCTime

deriveLiftMany [''Fixed, ''DiffTime, ''Day, ''UTCTime]

data MaskedDate
  = MaskedDate !Int
  | UnmaskedDate !Date

-- | Extract year part of a date value
dateYear :: Date -> Int
dateYear d = fromInteger y where (y,_,_) = toGregorian d

-- | Mask a date value by only keeping the year portion
maskDate :: Date -> MaskedDate
maskDate = MaskedDate . dateYear

-- | Lift a raw date value into a MaskedDate (either actually masked or unmasked)
maskDateIf :: Bool -> Date -> MaskedDate
maskDateIf True = maskDate
maskDateIf False = UnmaskedDate

-- | Extract year from a potentially masked date value
maskedYear :: MaskedDate -> Int
maskedYear (MaskedDate y) = y
maskedYear (UnmaskedDate d) = dateYear d

-- | Provide behavior to hook into general date formatting utilities
instance FormatTime MaskedDate where
  formatCharacter 'D' = Just (\locale _ -> formatTime locale "%m/%d/%y")
  formatCharacter 'F' = Just (\locale _ -> formatTime locale "%Y-%m-%d")
  formatCharacter 'x' = Just (\locale _ -> formatTime locale (dateFmt locale))
  formatCharacter c = f <$> formatCharacter c where
    f g l o (UnmaskedDate d) = g l o d
    f g l o (MaskedDate y)
      | c `elem` "YyCGgf" = r
      | otherwise = map (const 'X') r
      where r = g l o $ fromGregorian (toInteger y) 11 21

instance JSON.ToJSON MaskedDate where
  toJSON (MaskedDate y) = JSON.toJSON y
  toJSON (UnmaskedDate d) = JSON.toJSON d
