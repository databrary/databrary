module TestHarness
    (
      mkSolrIndexingContextSimple
    , ContextFor(..)
    , mkBackgroundContext
    , TestContext ( .. )
    , mkRequest
    , withStorage
    , withAV
    , mkAVStub
    , mkMailerMock
    , withTimestamp
    , withLogs
    , mkLogsStub
    , mkNotificationsStub
    , withInternalStateVal
    , mkDbContext
    , runContextReaderT
    , withinTestTransaction
    , stepsWithTransaction
    , stepsWithResourceAndTransaction
    , connectTestDb
    , makeSuperAdminContext
    , fakeIdentSessFromAuth
    , addAuthorizedInstitution
    , addAuthorizedInvestigatorWithInstitution
    , addAuthorizedInvestigator
    , addAffiliate
    , lookupSiteAuthNoIdent
    , switchIdentity
    -- , addAuthorization
    -- * re-export for convenience
    , runReaderT
    , Wai.defaultRequest
    , Id(..)
    , Identity(..)
    )
    where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.IORef (newIORef, IORef, modifyIORef)
import Control.Monad.Trans.Resource
    (InternalState, runResourceT, withInternalState, ResourceT, allocate)
import Data.Maybe
import Data.Time
import Database.PostgreSQL.Typed.Protocol
import qualified Hedgehog.Gen as Gen
import Network.Mail.Mime (Mail)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS hiding (unpack)
import qualified Data.ByteString.Char8 as BS
import qualified Network.Wai as Wai
import System.Directory (removeDirectoryRecursive)
import System.Posix.ByteString (mkdtemp, getEnvDefault, createDirectory)
import System.Posix.FilePath ((</>))

import Context
import EZID.Service (initEZID)
import Has
import HTTP.Client
import Ingest.Service (initIngest)
import Model.Authorize
import Model.Factories
import Model.Id
import Model.Identity
import Model.Party
import Model.Permission
import Model.Time
import Model.Token
import Service.DB
import Service.Entropy
import Service.Log
import Service.Mail (Mailer(..))
import Service.Messages (loadMessages, Messages)
import Service.Notification (initNotifications, Notifications)
import Service.Passwd (initPasswd)
import Service.Types
import Solr.Service (Solr(..))
import Static.Service (Static(..))
import Store.AV
import Store.Config as C (load, (!))
import Store.Service
import Store.Transcoder (initTranscoder)
import Store.Types (Storage(..), TranscoderConfig (..))
import Web.Types (Web(..))

-- Runtime dependencies
--   database started tests run
--   /databrary.conf has db credentials
--   database has seed data from 0.sql present
--   data file /test/data/small.webm present
--   TODO: desire to have passwd dict file in path
--   store directories present in /
--   /transcode, /transctl present
--   solr started using /solr-6.6.0 binaries w/core and config in /solr; databrary_logs created
--   ffmpeg exe on path
--   active internet connection for live http calls like geoname lookup to function

-- | Build the specialized context needed for solr indexing to run, with defaults for simple values
mkSolrIndexingContextSimple :: PGConnection -> Solr -> IO SolrIndexingContext
mkSolrIndexingContextSimple cn solr = do
    conf <- C.load "databrary.conf"
    logs <- initLogs (conf C.! "log")
    httpc <- initHTTPClient
    pure
      SolrIndexingContext {
               slcLogs = logs
             , slcHTTPClient = httpc
             , slcSolr = solr
             , slcDB = cn
             }

data ContextFor = ForEzid

-- | Build a specialized context needed to background jobs that use a small subset of services
mkBackgroundContext :: ContextFor -> InternalState -> PGConnection -> IO BackgroundContext
mkBackgroundContext ctxtFor ist cn =
    -- TODO: make a smaller context for ezid to use, so stubs aren't needed
    -- stubs
    let stubSolr = Solr { solrRequest = error "no solr req", solrProcess = Nothing }
        stubStorage = Storage undefined Nothing undefined undefined Nothing Nothing Nothing
        stubStatic = Static "" "" Nothing (\_ -> error "no val")
        stats = error "siteStats"
        stubWeb = Web {}
    in do
    conf <- C.load "databrary.conf"
    logs <- initLogs (conf C.! "log")
    mailer <- pure mkMailerStub
    httpc <- initHTTPClient
    (solr, ezid) <- case ctxtFor of
          ForEzid -> do
              ezidInst <- initEZID (conf C.! "ezid")
              pure (stubSolr, ezidInst)
    stubEntropy <- initEntropy
    stubPasswd <- initPasswd
    stubMessages <- loadMessages
    stubDb <- initDB (conf C.! "db")
    stubAv <- initAV
    stubIngest <- initIngest
    stubNotify <- initNotifications (conf C.! "notification")
    stubStatsref <- newIORef stats
    let
        service = Service {
                        serviceStartTime = UTCTime (fromGregorian 2018 3 4) (secondsToDiffTime 0)
                      , serviceSecret = Secret "abc"
                      , serviceEntropy = stubEntropy
                      , servicePasswd = stubPasswd
                      , serviceLogs = logs
                      , serviceMailer = mailer
                      , serviceMessages = stubMessages
                      , serviceDB = stubDb
                      , serviceStorage = stubStorage
                      , serviceAV = stubAv
                      , serviceWeb = stubWeb
                      , serviceHTTPClient = httpc
                      , serviceStatic = stubStatic
                      , serviceStats = stubStatsref
                      , serviceIngest = stubIngest
                      , serviceSolr = solr
                      , serviceEZID = ezid
                      , servicePeriodic = Nothing
                      , serviceNotification = stubNotify
                      , serviceDown = Nothing
                      }
        actionContext = ActionContext {
                            contextService = service
                          , contextTimestamp = UTCTime (fromGregorian 2018 4 5) (secondsToDiffTime 0)
                          , contextResourceState = ist
                          , contextDB = cn
                          }
    pure (BackgroundContext actionContext)

-- |
-- "God object" that can fulfill all needed "Has" instances. This is
-- intentionally quick to use for tests. The right way to use it is to keep all
-- fields undefined except those that the test in question is using: Since
-- runtime for tests is just as good as compile time for library, any bottoms
-- encountered will be a "good crash".
data TestContext = TestContext
    { ctxRequest :: Maybe Wai.Request
    -- ^ for MonadHasRequest
    , ctxSecret :: Maybe Secret
    , ctxEntropy :: Maybe Entropy
    -- ^ Both for MonadSign
    , ctxPartyId :: Maybe (Id Party)
    -- ^ for MonadAudit
    , ctxConn :: Maybe DBConn
    -- ^ for MonadDB
    , ctxStorage :: Maybe Storage
    -- ^ for MonadStorage
    , ctxSolr :: Maybe Solr
    -- ^ for MonadSolr
    , ctxMailer :: Maybe Mailer
    -- ^ for MonadMail
    , ctxInternalState :: Maybe InternalState
    , ctxIdentity :: Maybe Identity
    , ctxSiteAuth :: Maybe SiteAuth
    , ctxAV :: Maybe AV
    , ctxTimestamp :: Maybe Timestamp
    , ctxLogs :: Maybe Logs
    , ctxHttpClient :: Maybe HTTPClient
    , ctxNotifications :: Maybe Notifications
    , ctxMessages :: Maybe Messages
    }

blankContext :: TestContext
blankContext = TestContext
    { ctxRequest = Nothing
    , ctxSecret = Nothing
    , ctxEntropy = Nothing
    , ctxPartyId = Nothing
    , ctxConn = Nothing
    , ctxStorage = Nothing
    , ctxSolr = Nothing
    , ctxMailer = Nothing
    , ctxInternalState = Nothing
    , ctxIdentity = Nothing
    , ctxSiteAuth = Nothing
    , ctxAV = Nothing
    , ctxTimestamp = Nothing
    , ctxLogs = Nothing
    , ctxHttpClient = Nothing
    , ctxNotifications = Nothing
    , ctxMessages = Nothing
    }

addCntxt :: TestContext -> TestContext -> TestContext
addCntxt c1 c2 =
    c1 {
          ctxRequest = ctxRequest c1 <|> ctxRequest c2
        , ctxSecret = ctxSecret c1 <|> ctxSecret c2
        , ctxEntropy = ctxEntropy c1 <|> ctxEntropy c2
        , ctxPartyId = ctxPartyId c1 <|> ctxPartyId c2
        , ctxConn = ctxConn c1 <|> ctxConn c2
        , ctxStorage = ctxStorage c1 <|> ctxStorage c2
        , ctxSolr = ctxSolr c1 <|> ctxSolr c2
        , ctxMailer = ctxMailer c1 <|> ctxMailer c2
        , ctxInternalState = ctxInternalState c1 <|> ctxInternalState c2
        , ctxIdentity = ctxIdentity c1 <|> ctxIdentity c2
        , ctxSiteAuth = ctxSiteAuth c1 <|> ctxSiteAuth c2
        , ctxAV = ctxAV c1 <|> ctxAV c2
        , ctxTimestamp = ctxTimestamp c1 <|> ctxTimestamp c2
        , ctxLogs = ctxLogs c1 <|> ctxLogs c2
        , ctxHttpClient = ctxHttpClient c1 <|> ctxHttpClient c2
        , ctxNotifications = ctxNotifications c1 <|> ctxNotifications c2
        , ctxMessages = ctxMessages c1 <|> ctxMessages c2
       }

instance Has Identity TestContext where
    view = fromJust . ctxIdentity

instance Has DBConn TestContext where
    view = fromJust . ctxConn

instance Has Wai.Request TestContext where
    view = fromJust . ctxRequest

instance Has Secret TestContext where
    view = fromJust . ctxSecret

instance Has Entropy TestContext where
    view = fromJust . ctxEntropy

instance Has AV TestContext where
    view = fromJust . ctxAV

instance Has Solr TestContext where
    view = fromJust . ctxSolr

instance Has Storage TestContext where
    view = fromJust . ctxStorage

instance Has InternalState TestContext where
    view = fromJust . ctxInternalState

instance Has Timestamp TestContext where
    view = fromJust . ctxTimestamp

instance Has Logs TestContext where
    view = fromJust . ctxLogs

instance Has Mailer TestContext where
    view = fromJust . ctxMailer

instance Has HTTPClient TestContext where
    view = fromJust . ctxHttpClient

instance Has Notifications TestContext where
    view = fromJust . ctxNotifications

instance Has Messages TestContext where
    view = fromJust . ctxMessages

instance Has Party TestContext where
    view = view . fromJust . ctxSiteAuth

instance Has Account TestContext where
    view = view . fromJust . ctxSiteAuth

-- Needed for types, but unused so far

-- prefer using SiteAuth instead of Identity for test contexts
instance Has SiteAuth TestContext where
    view = fromJust . ctxSiteAuth

instance Has (Id Party) TestContext where
    view = fromJust . ctxPartyId

instance Has Access TestContext where
    view = view . fromJust . ctxIdentity

mkRequest :: Wai.Request
mkRequest = Wai.defaultRequest { Wai.requestHeaderHost = Just "invaliddomain.org" }

-- | Create a temp dir and use it for all Storage fields
withStorage :: ResourceT IO Storage
withStorage = do
    -- XDG_RUNTIME_DIR, TMPDIR, or "/tmp", whichever is available first
    userTmpDir <- liftIO
        (getEnvDefault "XDG_RUNTIME_DIR" =<< getEnvDefault "TMPDIR" "/tmp")
    -- Explicitly drop the release key, since the ResourceT in which this runs
    -- will always be short-lived.
    (_, dir) <- allocate
        (mkdtemp (userTmpDir </> "databrary-storage-fixture-"))
        (removeDirectoryRecursive . BS.unpack)
    -- initStorage and initTranscoder expect these to exist.
    (liftIO  . mapM_ (flip createDirectory 0o755 . (dir </>)))
        ["tmp", "stage", "upload", "trans", "cache"]
    let tc = initTranscoder TranscoderConfig
            { transcoderHost = Nothing
            , transcoderDir = Just (BS.unpack (dir </> "trans"))
            , transcoderMount = Nothing
            }
    (liftIO . mkStorage tc) dir
  where
    mkStorage tc dir = (flip initStorage tc . Right) StorageLocationConfig
        { storageLocTemp = Just (dir </> "tmp")
        , storageLocMaster = dir </> "stage"
        , storageLocUpload = dir </> "upload"
        , storageLocCache = Just (dir </> "cache")
        , storageLocStage = Just (dir </> "stage")
        , storageLocFallback = Nothing
        }

withAV :: TestContext -> IO TestContext
withAV ctxt = do
    addCntxt ctxt <$> mkAVContext

withTimestamp :: Timestamp -> TestContext -> TestContext
withTimestamp ts ctxt =
    addCntxt ctxt (blankContext { ctxTimestamp = Just ts })

withLogs :: TestContext -> IO TestContext
withLogs ctxt = do
    logs <- initLogs mempty
    pure (addCntxt ctxt (blankContext { ctxLogs = Just logs }))

mkLogsStub :: IO Logs
mkLogsStub = initLogs mempty

mkNotificationsStub :: IO Notifications
mkNotificationsStub = do
    conf <- C.load "databrary.conf" -- alternatively, don't use config
    initNotifications (conf C.! "notification")

mkAVContext :: IO TestContext
mkAVContext = do
    av <- initAV
    pure (blankContext { ctxAV = Just av })

mkAVStub :: IO AV   -- currently the same as initAV, but in the future can become simpler
mkAVStub = initAV

mkMailerMock :: IO (Mailer, IORef [Mail]) -- TODO: change to StateT?
mkMailerMock = do
    ref <- newIORef []
    mailer <- pure (Mailer (\_ _ _ _ mail -> modifyIORef ref (\ls -> ls ++ [mail])))
    pure (mailer, ref)

mkMailerStub :: Mailer
mkMailerStub = Mailer (\_ _ _ _ _ -> pure ())

withInternalStateVal :: InternalState -> TestContext -> TestContext
withInternalStateVal ist ctxt =
    addCntxt ctxt (blankContext { ctxInternalState = Just ist })

-- | Convenience for building a context with only a db connection
mkDbContext :: DBConn -> TestContext
mkDbContext c = blankContext { ctxConn = Just c }

-- | Convenience for runReaderT where context consists of db connection only
runContextReaderT :: DBConn -> ReaderT TestContext IO a -> IO a
runContextReaderT cn rdrActions = runReaderT rdrActions (blankContext { ctxConn = Just cn })

-- | Run an action that uses a db connection and also needs to use internal state/resourcet.
-- TODO: make this cleaner.
withinResourceAndTransaction :: (InternalState -> PGConnection -> IO a) -> IO a
withinResourceAndTransaction act =
    runResourceT $ withInternalState $ \ist ->
        withinTestTransaction (act ist)

-- | Execute a test within a DB connection that rolls back at the end.
withinTestTransaction :: (PGConnection -> IO a) -> IO a
withinTestTransaction act =
     bracket
         (do
              cn <- pgConnect =<< loadPGDatabase
              pgBegin cn
              pure cn)
         (\cn -> pgRollback cn >> pgDisconnect cn)
         act

-- | Combine 'testCaseSteps' and 'withinResourceandTransaction'
stepsWithResourceAndTransaction
    :: TestName -> ((String -> IO ()) -> InternalState -> PGConnection -> IO ()) -> TestTree
stepsWithResourceAndTransaction name f =
    testCaseSteps name (\step -> withinResourceAndTransaction (f step))

-- | Combine 'testCaseSteps' and 'withinTestTransaction'
stepsWithTransaction
    :: TestName -> ((String -> IO ()) -> PGConnection -> IO ()) -> TestTree
stepsWithTransaction name f =
    testCaseSteps name (\step -> withinTestTransaction (f step))

connectTestDb :: IO PGConnection
connectTestDb =
    loadPGDatabase >>= pgConnect

makeSuperAdminContext :: PGConnection -> BS.ByteString -> IO TestContext -- login + spawn context
makeSuperAdminContext cn adminEmail =
    runReaderT
        (do
             Just auth <- lookupSiteAuthByEmail False adminEmail
             let pid = (partyId . partyRow . accountParty . siteAccount) auth
                 ident = fakeIdentSessFromAuth auth True
             pure (blankContext {
                        ctxConn = Just cn
                      , ctxIdentity = Just ident
                      , ctxSiteAuth = Just (view ident)
                      , ctxPartyId = Just pid
                      , ctxRequest = Just Wai.defaultRequest
                      }))
        blankContext { ctxConn = Just cn }

fakeIdentSessFromAuth :: SiteAuth -> Bool -> Identity
fakeIdentSessFromAuth a su =
    Identified
      (Session
         (AccountToken (Token (Id "id") (UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0))) a)
         "verf"
         su)

addAuthorizedInstitution :: TestContext -> IO Party  -- create + approve as site admin
addAuthorizedInstitution adminCtxt = do
    createInst <- Gen.sample genCreateInstitutionParty
    runReaderT
        (do
             created <- addParty createInst
             changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing created rootParty)
             pure created)
        adminCtxt

-- TODO: receive expiration date (expiration dates might not be used...)  -- register as anon + approve as site admin
addAuthorizedInvestigator :: TestContext -> Party -> IO Account
addAuthorizedInvestigator adminCtxt instParty = do
    let ctxtNoIdent =
          adminCtxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    a <- Gen.sample genAccountSimple
    aiAccount <-
        runReaderT
            (do
                 created <- addAccount a
                 Just auth <- lookupSiteAuthByEmail False (accountEmail a)
                 changeAccount (auth { accountPasswd = Just "somehashedvalue" })
                 pure created)
            ctxtNoIdent
    runReaderT
        (changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing (accountParty aiAccount) instParty))
        adminCtxt
    pure aiAccount

-- registerConfirm :: DBConn -> IO Account
-- registerConfirm cn = do
    -- some account w/email
    -- runWithoutIdent addAccount + lookupAuth + changeAccount

addAuthorizedInvestigatorWithInstitution :: DBConn -> BS.ByteString -> IO (Account, TestContext)
addAuthorizedInvestigatorWithInstitution cn adminEmail = do
    ctxt <- makeSuperAdminContext cn adminEmail
    instParty <- addAuthorizedInstitution ctxt
    aiAcct <- addAuthorizedInvestigator ctxt instParty

    -- login as AI, bld cntxt
    let ctxtNoIdent = ctxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    Just aiAuth <- runReaderT (lookupSiteAuthByEmail False (accountEmail aiAcct)) ctxtNoIdent
    let aiCtxt = switchIdentity ctxt aiAuth False
    pure (aiAcct, aiCtxt)

-- TODO: receive expiration date    -- register as anon + approve as ai
addAffiliate :: TestContext -> Party -> Permission -> Permission -> IO Account
addAffiliate aiCntxt aiParty site member = do
    let ctxtNoIdent =
          aiCntxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    a <- Gen.sample genAccountSimple
    affAccount <-
        runReaderT
            (do
                 created <- addAccount a
                 Just auth <- lookupSiteAuthByEmail False (accountEmail a)
                 changeAccount (auth { accountPasswd = Just "somehashedvalue" })
                 pure created)
            ctxtNoIdent
    runReaderT
        (changeAuthorize (makeAuthorize (Access site member) Nothing (accountParty affAccount) aiParty))
        aiCntxt
    pure affAccount

{-
-- TODO: receive authorization
addAuthorization :: TestContext -> Party -> Party -> Permission -> Permission -> IO ()
addAuthorization ctxt parentParty requestParty site member = do
    runReaderT
        (changeAuthorize (makeAuthorize (Access site member) Nothing requestParty parentParty))
        ctxt
-}

lookupSiteAuthNoIdent :: TestContext -> BS.ByteString -> IO SiteAuth
lookupSiteAuthNoIdent privCtxt email = do
    let ctxtNoIdent =
          privCtxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    fromJust <$> runReaderT (lookupSiteAuthByEmail False email) ctxtNoIdent

switchIdentity :: TestContext -> SiteAuth -> Bool -> TestContext
switchIdentity baseCtxt auth su = do
    baseCtxt {
          ctxIdentity = Just (fakeIdentSessFromAuth auth su)
        , ctxPartyId = Just ((partyId . partyRow . accountParty . siteAccount) auth)
        , ctxSiteAuth = Just auth
    }
