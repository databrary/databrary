{-# LANGUAGE OverloadedStrings #-}
module Web.Files
  ( allWebFiles
  , findWebFiles
  ) where

import Control.Exception (bracket)
import Control.Monad (ap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Posix.Directory.ByteString (openDirStream, closeDirStream)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (readDirEnt)
import System.Posix.FilePath (takeExtensions)

import Files
import Web

listFiles :: RawFilePath -> IO [RawFilePath]
listFiles dir = loop "" where
  loop b = bracket
    (openDirStream (dir </> b))
    closeDirStream
    (ent b)
  ent b dh = do
    (t, f) <- readDirEnt dh
    if BS.null f
      then return []
      else ap
        (if     BSC.head f == '.'
          then return id
        else if t == dtDir
          then (++) <$> loop (b </> f)
        else if t == dtReg
          then return $ (:) (b </> f)
        else   return id)
        (ent b dh)

allWebFiles :: IO [WebFilePath]
allWebFiles = withWebDir $ \webDirRaw -> mapM makeWebFilePath =<< listFiles webDirRaw

findWebFiles :: BS.ByteString -> IO [WebFilePath]
findWebFiles ext = filter ((ext ==) . takeExtensions . webFileRel) <$> allWebFiles
