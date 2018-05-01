{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving
   , TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
module Databrary.Model.PartyTest where

-- import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
-- import Control.Monad.Reader
import Data.Maybe
-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Service.DB

unit_partyName_example :: Assertion
unit_partyName_example = do
    (partyName . partyRow) nobodyParty @?= "Everybody"

runLookupParty :: Id Party -> Identity -> Maybe Party -> Assertion
runLookupParty pid ident expected = do
    cn <- loadPGDatabase >>= pgConnect
    let ctxt = Context cn ident
    -- 
    mParty <- runReaderT (lookupParty pid :: ReaderT Context IO (Maybe Party)) ctxt
    mParty @?= expected

unit_lookupParty :: Assertion
unit_lookupParty = do
    runLookupParty (Id 2) NotLoggedIn (Just staffParty)
    -- TODO: Fix these with real data parameters
    -- runLookupParty (Id 2) (Identified undefined) (Just staffParty)
    -- runLookupParty (Id 2) (ReIdentified undefined) (Just staffParty)

unit_lookupSiteAuthByEmail :: Assertion
unit_lookupSiteAuthByEmail = do
    cn <- loadPGDatabase >>= pgConnect
    let ctxt = Context { ctxConn = cn }
    mAuth <- runReaderT (lookupSiteAuthByEmail False "test@databrary.org") ctxt
    isJust mAuth @? "should find the well known test user's site auth by email"
    mAuth' <- runReaderT (lookupSiteAuthByEmail False "doesntexist@databrary.org") ctxt
    mAuth' @?= Nothing
    mAuth'' <- runReaderT (lookupSiteAuthByEmail True "TEST@DATABRARY.ORG") ctxt
    isJust mAuth'' @? "should find the well known test user's site auth by email, case insensitive"
    

instance Has DBConn Context where
    view = ctxConn

instance Has Identity Context where
    view = ctxIdentity

instance Has SiteAuth Context where
    view = undefined

instance Has Party Context where
    view = undefined

instance Has (Id Party) Context where
    view = undefined

instance Has Access Context where
    view = undefined

-- TODO: make Context generalized for all tests, more Has instances, all undefined by defined
data Context = Context
    { ctxConn :: DBConn
    , ctxIdentity :: Identity
    }
