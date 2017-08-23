{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (void)
#ifndef DEVEL
import Control.Monad.Reader (runReaderT)
#endif
import qualified Data.Aeson.Encode as J (encodeToBuilder)
import Data.ByteString.Builder (hPutBuilder)
import Data.Either (partitionEithers)
import qualified System.Console.GetOpt as Opt
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)

#ifndef DEVEL
import Paths_databrary (getDataFileName)
import Databrary.Service.Types (serviceDB)
import Databrary.Service.DB (withDB)
import Databrary.Service.DB.Schema (updateDBSchema)
#endif
import qualified Databrary.Store.Config as Conf
import Databrary.Service.Init (withService)
import Databrary.Context
import Databrary.Web.Rules (generateWebFiles)
import Databrary.Action (runActionRoute)
import Databrary.Routes (routeMap)
import Databrary.Routes.API (swagger)
import Databrary.Warp (runWarp)
import Databrary.EZID.Volume (updateEZID)

import Data.Monoid
import System.Directory
import System.Process
import qualified Data.Text as Text

data Flag
  = FlagConfig FilePath
  | FlagWeb
  | FlagAPI
  | FlagEZID
  deriving (Eq)

opts :: [Opt.OptDescr Flag]
opts =
  [ Opt.Option "c" ["config"] (Opt.ReqArg FlagConfig "FILE") "Path to configuration file [./databrary.conf]"
  , Opt.Option "w" ["webgen"] (Opt.NoArg FlagWeb) "Generate web assets only"
  , Opt.Option "a" ["api"] (Opt.NoArg FlagAPI) "Output Swagger API documention"
  , Opt.Option "e" ["ezid"] (Opt.NoArg FlagEZID) "Update EZID DOIs"
  ]

flagConfig :: Flag -> Either FilePath Flag
flagConfig (FlagConfig f) = Left f
flagConfig f = Right f

main :: IO ()
main = do
  putStrLn "Starting Main..."
  prog <- getProgName
  args <- getArgs
  let (flags, args', err) = Opt.getOpt Opt.Permute opts args
      (configs, flags') = partitionEithers $ map flagConfig flags

  conf <- mconcat <$> mapM Conf.load (case configs of
    [] -> ["databrary.conf"]
    l -> l)
  case (flags', args', err) of
    ([FlagWeb], [], []) -> do
      putStrLn "generating files..." 
      void generateWebFiles
      putStrLn "finished generating web files..."
      exitSuccess
    ([FlagAPI], [], []) -> do
      putStrLn "put web builder..."
      hPutBuilder stdout $ J.encodeToBuilder swagger
      putStrLn "finished web builder..."
      exitSuccess
    ([FlagEZID], [], []) -> do
      putStrLn "update EZID..."
      r <- withService False conf $ runContextM $ withBackgroundContextM updateEZID
      putStrLn "update EZID finished..."
      if r == Just True then exitSuccess else exitFailure
    ([], [], []) -> do 
      putStrLn "No flags or args...."
      return ()
    _ -> do
      mapM_ putStrLn err
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") opts
      exitFailure

  putStrLn "evaluating routemap..."
  routes <- evaluate routeMap
  putStrLn "evaluating routemap...withService..."
  withService True conf $ \rc -> do
#ifndef DEVEL
    --schema <- getDataFileName "schema"
    let  schema = "./schema" :: FilePath
    putStrLn "updating schema"
    withDB (serviceDB rc) $ runReaderT $ updateDBSchema schema
    putStrLn "updating schema completed"
#endif
    putStrLn "running warp"
    runWarp conf rc (runActionRoute routes rc)
