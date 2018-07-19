{-# LANGUAGE OverloadedStrings #-}
module Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad.IO.Class (liftIO)
import System.Process (callProcess)
import System.FilePath (takeExtensions)

import Files
import Web
import Web.Types
import Web.Files
import Web.Generate

generateStylusCSS :: WebGenerator
generateStylusCSS = \fo@(f, _) -> do
  let src = "app.styl"
  sl <- liftIO $ findWebFiles ".styl"
  fpRel <- liftIO $ unRawFilePath $ webFileRel f
  fpAbs <- liftIO $ unRawFilePath $ webFileAbs f
  srcAbs <- liftIO $ (unRawFilePath . webFileAbs) =<< makeWebFilePath =<< rawFilePath src
  webRegenerate
    (callProcess
      "stylus" $
    (if takeExtensions fpRel == ".min.css" then ("-c":) else id)
    [ "-u", "nib"
    , "-u", "autoprefixer-stylus"
    , "-o", fpAbs
    , srcAbs
    ])
    []
    sl
    fo
