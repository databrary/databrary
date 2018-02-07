{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.PermissionUtil
  ( maskRestrictedString 
  ) where

import qualified Data.String as ST

maskRestrictedString :: ST.IsString a => a -> a
maskRestrictedString s = (const (ST.fromString "")) s
