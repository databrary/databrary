{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad.IO.Class (liftIO)
import System.Process (callProcess)
import System.FilePath (takeExtensions)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate
import qualified Databrary.Store.Config as Conf

generateStylusCSS :: WebGenerator
generateStylusCSS fo@(f, _) = do
  let src = "app.styl"
  sl <- liftIO $ findWebFiles ".styl"
  nodeModulesPath <- liftIO $ Conf.get "node.modules.path" <$> Conf.getConfig
  let stylusBinPath = nodeModulesPath </> ".bin" </> "stylus"
  let cssFilter = if takeExtensions (webFileRel f) == ".min.css" then ("-c":) else id
  let stylusArgs = ["-u", "nib", "-u", "autoprefixer-stylus", "-o", webFileAbs f, webFileAbs src]
  webRegenerate (callProcess stylusBinPath $ cssFilter stylusArgs) [] sl fo
