module Databrary.Web.Generate
  ( fileNewer
  , staticWebGenerate
  , webRegenerate
  , webLinkDataFile
  ) where

import Control.Monad (when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName, takeDirectory)
import System.Posix.Files (createSymbolicLink, rename)
import System.Process

import Databrary.Files
import Databrary.Model.Time
import Databrary.Web
import Databrary.Web.Types
import {-# SOURCE #-} Databrary.Web.Rules

anyM :: Monad m => [m Bool] -> m Bool
anyM [] = return False
anyM (a:l) = do
  r <- a
  if r then return True else anyM l

fileNotFound :: IsFilePath f => f -> WebGeneratorM a
fileNotFound f = throwError $ toFilePath f ++ " not found\n"

fileNewerThan :: IsFilePath f => Timestamp -> f -> WebGeneratorM Bool
fileNewerThan t f =
  maybe (fileNotFound f) (return . (t <) . snd) =<< liftIO (fileInfo f)

fileNewer :: IsFilePath f => f -> WebGenerator
fileNewer f (_, Nothing) = do
  e <- liftIO $ fileExist f
  unless e $ fileNotFound f
  return True
fileNewer f (_, Just o) =
  fileNewerThan (webFileTimestamp o) f

whether :: Bool -> IO () -> IO Bool
whether g = (g <$) . when g

webRegenerate :: IO () -> [FilePath] -> [WebFilePath] -> WebGenerator
webRegenerate g fs ws (f, o) = do
  wr <- mapM (generateWebFile False) ws
  ft <- liftIO $ maybe (fmap snd <$> fileInfo f) (return . Just . webFileTimestamp) o
  fr <- maybe (return False) (\t -> anyM $ map (fileNewerThan t) fs) ft
  liftIO $ whether (all (\t -> fr || any ((t <) . webFileTimestamp) wr) ft) g

staticWebGenerate :: (FilePath -> IO ()) -> WebGenerator
staticWebGenerate g (w, _) = liftIO $ do
  g t
  c <- catchDoesNotExist $ compareFiles f t
  if or c
    then False <$ removeLink t
    else True <$ rename t f
  where
  f = toFilePath w
  (d, n) = splitFileName f
  t = d </> ('.' : n)

webLinkDataFile :: FilePath -> WebGenerator
webLinkDataFile s fo@(f, _) = do
  nodeDeps <- liftIO $ readCreateProcess
    (shell "nix-build ./node-default.nix --no-out-link -A shell.nodeDependencies")
    ""
  let nodeDeps' = T.unpack $ T.strip $ T.pack $ nodeDeps
  let wf = nodeDeps' </> "lib" </> s
  liftIO $ print wf
  webRegenerate (do
    r <- removeFile f
    unless r $ createDirectoryIfMissing False $ takeDirectory (webFileAbs f)
    createSymbolicLink wf (webFileAbs f))
    [wf] [] fo
