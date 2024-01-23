{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Model.Party
  ( module Model.Party.Types
  , partyName
  , partyEmail
  , lookupParty
  , isNobodyParty
  , lookupPartyAuthorizations
  , lookupAuthParty
  , lookupSiteAuthByEmail
  , changeParty
  , changeAccount
  , addParty
  , addAccount
  , removeParty
  , auditAccountLogin
  , recentAccountLogins
  , partyRowJSON
  , partyJSON
  , toFormattedParty
  , PartyFilter(..)
  , findParties
  , lookupAvatar
  , changeAvatar
  , getDuplicateParties
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.String
import qualified Data.Text as T
-- import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep, pgLiteralString, pgSafeLiteral)
import Database.PostgreSQL.Typed.Types

import Ops
import Has (Has(..), peek)
import Service.DB
import qualified JSON
import HTTP.Request
import Model.Id
import Model.SQL
import Model.Paginate
import Model.Paginate.SQL
import Model.Permission
import Model.Audit
-- import Model.Audit.SQL
import Model.Identity.Types
import Model.Volume
import Model.Asset.Types
import Model.Asset.SQL
import Model.Party.Types
import Model.Party.SQL
import Model.URL (URI)

useTDB

partyName :: PartyRow -> T.Text
partyName PartyRow{ partyPreName = Just p, partySortName = n } = p <> T.cons ' ' n
partyName PartyRow{ partySortName = n } = n

emailPermission :: Permission
emailPermission = PermissionSHARED

showEmail :: Identity -> Bool
showEmail i = accessSite i >= emailPermission

partyEmail :: Party -> Maybe BS.ByteString
partyEmail p =
  guard (partyPermission p >= emailPermission) >> accountEmail <$> partyAccount p

-- | Core party object with formatting and authorization applied, ready for
-- JSON output
data FormattedParty = FormattedParty
    { fpyId :: !Int32
    , fpySortname :: !T.Text
    , fpyPrename :: !(Maybe T.Text)
    , fpyOrcid :: !(Maybe String)
    , fpyAffiliation :: !(Maybe T.Text)
    , fpyUrl :: !(Maybe URI)
    , fpyInstitution :: !(Maybe Bool)
    , fpyEmail :: !(Maybe BS.ByteString)
    , fpyPermission :: !(Maybe Permission)
    , fpyAuthorization :: !(Maybe Permission)
    }

instance JSON.ToJSON FormattedParty where
    toJSON FormattedParty{..} =  -- Bryan: if you want to use a fancy generic transform?
        JSON.object (
               ["id" JSON..= fpyId]
            <> ["sortname" JSON..= fpySortname]
            <> "prename" `JSON.omitIfNothing` fpyPrename
            <> "orcid" `JSON.omitIfNothing` fpyOrcid
            <> "affiliation" `JSON.omitIfNothing` fpyAffiliation
            <> "url" `JSON.omitIfNothing` fpyUrl
            <> "institution" `JSON.omitIfNothing` fpyInstitution
            <> "email" `JSON.omitIfNothing` fpyEmail
            <> "permission" `JSON.omitIfNothing` fpyPermission
            <> "authorization" `JSON.omitIfNothing` fpyAuthorization)

partyRowJSON :: JSON.ToObject o => PartyRow -> JSON.Record (Id Party) o
partyRowJSON PartyRow{..} = JSON.Record partyId $
     "sortname" JSON..= partySortName
  <> "prename" `JSON.kvObjectOrEmpty` partyPreName
  <> "orcid" `JSON.kvObjectOrEmpty` (show <$> partyORCID)
  <> "affiliation" `JSON.kvObjectOrEmpty` partyAffiliation
  <> "url" `JSON.kvObjectOrEmpty` partyURL

partyJSON :: JSON.ToObject o => Party -> JSON.Record (Id Party) o
partyJSON p@Party{..} = partyRowJSON partyRow `JSON.foldObjectIntoRec`
 (   "institution" `JSON.kvObjectOrEmpty` (True `useWhen` isNothing partyAccount)
  <> "email" `JSON.kvObjectOrEmpty` partyEmail p
  <> "permission" `JSON.kvObjectOrEmpty` (partyPermission `useWhen` (partyPermission > PermissionREAD)))

-- | Apply formatting and authorization to a core Party object, replacing partyJSON gradually
toFormattedParty :: Party -> FormattedParty
toFormattedParty p@Party{..} = FormattedParty {
      fpyId = unId (partyId partyRow)
    , fpySortname = partySortName partyRow
    , fpyPrename = partyPreName partyRow
    , fpyOrcid = show <$> partyORCID partyRow
    , fpyAffiliation = partyAffiliation partyRow
    , fpyUrl = partyURL partyRow
    , fpyInstitution = True `useWhen` isNothing partyAccount
    , fpyEmail = partyEmail p
    , fpyPermission = partyPermission `useWhen` (partyPermission > PermissionREAD)
    , fpyAuthorization = loadedToMaybe partySiteAccess
    }

changeParty :: MonadAudit c m => Party -> m ()
changeParty p = do
  ident <- getAuditIdentity
  let _tenv_a6PEM = unknownPGTypeEnv
  dbExecute1' -- (updateParty 'ident 'p)
   (mapQuery2
    ((\ _p_a6PEN _p_a6PEO _p_a6PEP _p_a6PEQ _p_a6PER _p_a6PES _p_a6PET ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE party SET name=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEN,
                        Data.String.fromString ",prename=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEO,
                        Data.String.fromString ",affiliation=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEP,
                        Data.String.fromString ",url=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEQ,
                        Data.String.fromString " WHERE id=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PER,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.party SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PES,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6PET,
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"]))
      (partySortName $ partyRow p)
      (partyPreName $ partyRow p)
      (partyAffiliation $ partyRow p)
      (partyURL $ partyRow p)
      (partyId $ partyRow p)
      (auditWho ident)
      (auditIp ident))
    (\ [] -> ()))

changeAccount :: MonadAudit c m => SiteAuth -> m ()
changeAccount a = do
  ident <- getAuditIdentity
  let _tenv_a6PFv = unknownPGTypeEnv
  dbExecute1' -- (updateAccount 'ident 'a)
   (mapQuery2
    ((\ _p_a6PFw _p_a6PFx _p_a6PFy _p_a6PFz _p_a6PFA ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE account SET email=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a6PFw,
                        Data.String.fromString ",password=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a6PFx,
                        Data.String.fromString " WHERE id=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFy,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.account SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFz,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6PFA,
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"]))
      (accountEmail (siteAccount a))
      (accountPasswd a)
      (partyId $ partyRow (accountParty (siteAccount a)))
      (auditWho ident)
      (auditIp ident))
    (\[] -> ()))

-- | Create a new party without an account, intended for creating institution parties.
addParty :: MonadAudit c m => Party -> m Party
addParty bp = do
  ident <- getAuditIdentity
  -- Similar to add account, load resulting party with default values for party permission and
  -- access.
  let _tenv_a6PKN = unknownPGTypeEnv
  row <- dbQuery1' -- (insertParty 'ident 'bp)
    (mapQuery2
      ((\ _p_a6PKO _p_a6PKP _p_a6PKQ _p_a6PKR _p_a6PKS _p_a6PKT ->
                       (BS.concat
                          [Data.String.fromString
                             "WITH audit_row AS (INSERT INTO party (name,prename,affiliation,url) VALUES (",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKO,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKP,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKQ,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKR,
                           Data.String.fromString
                             ") RETURNING *) INSERT INTO audit.party SELECT CURRENT_TIMESTAMP, ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a6PKS,
                           Data.String.fromString ", ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                             _p_a6PKT,
                           Data.String.fromString
                             ", 'add'::audit.action, * FROM audit_row RETURNING party.id,party.name,party.prename,party.orcid,party.affiliation,party.url"]))
         (partySortName $ partyRow bp)
         (partyPreName $ partyRow bp)
         (partyAffiliation $ partyRow bp)
         (partyURL $ partyRow bp)
         (auditWho ident)
         (auditIp ident))
               (\
                  [_cid_a6PKU,
                   _cname_a6PKV,
                   _cprename_a6PKX,
                   _corcid_a6PKY,
                   _caffiliation_a6PKZ,
                   _curl_a6PL0]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6PKU,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a6PKV,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a6PKX,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a6PKY,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a6PKZ,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a6PL0)))
  let pRow =
          (\ (vid_a6PKd, vname_a6PKe, vprename_a6PKf, vorcid_a6PKh,
             vaffiliation_a6PKi, vurl_a6PKj)
           -> PartyRow
                vid_a6PKd
                vname_a6PKe
                vprename_a6PKf
                vorcid_a6PKh
                vaffiliation_a6PKi
                vurl_a6PKj)
           row
  pure ((\p -> Party p Nothing NotLoaded PermissionREAD Nothing) pRow)

-- | Create a new account without any authorizations, during registration, using the nobodySiteAuth.
-- The account password will be blank. The party will not have any authorizations yet.
addAccount :: MonadAudit c m => Account -> m Account
addAccount ba@Account{ accountParty = bp } = do
  let _tenv_a6PKN = unknownPGTypeEnv
  ident <- getAuditIdentity
  -- Create a party. The account will be created below, so start with no account.
  -- Load resulting party with default values for party permission and access for now.
  row <- dbQuery1' --  fmap (\p -> Party p Nothing PermissionREAD Nothing) -- (insertParty 'ident 'bp)
   (mapQuery2
      ((\ _p_a6PKO _p_a6PKP _p_a6PKQ _p_a6PKR _p_a6PKS _p_a6PKT ->
                       (BS.concat
                          [Data.String.fromString
                             "WITH audit_row AS (INSERT INTO party (name,prename,affiliation,url) VALUES (",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKO,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKP,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKQ,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PKR,
                           Data.String.fromString
                             ") RETURNING *) INSERT INTO audit.party SELECT CURRENT_TIMESTAMP, ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a6PKS,
                           Data.String.fromString ", ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PKN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                             _p_a6PKT,
                           Data.String.fromString
                             ", 'add'::audit.action, * FROM audit_row RETURNING party.id,party.name,party.prename,party.orcid,party.affiliation,party.url"]))
         (partySortName $ partyRow bp)
         (partyPreName $ partyRow bp)
         (partyAffiliation $ partyRow bp)
         (partyURL $ partyRow bp)
         (auditWho ident)
         (auditIp ident))
               (\
                  [_cid_a6PKU,
                   _cname_a6PKV,
                   _cprename_a6PKX,
                   _corcid_a6PKY,
                   _caffiliation_a6PKZ,
                   _curl_a6PL0]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6PKU,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a6PKV,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a6PKX,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a6PKY,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a6PKZ,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PKN
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a6PL0)))
  let pRow =
           (\ (vid_a6PKd, vname_a6PKe, vprename_a6PKf, vorcid_a6PKh,
               vaffiliation_a6PKi, vurl_a6PKj)
              -> PartyRow
                   vid_a6PKd
                   vname_a6PKe
                   vprename_a6PKf
                   vorcid_a6PKh
                   vaffiliation_a6PKi
                   vurl_a6PKj)
           row
      p = (\pr -> Party pr Nothing NotLoaded PermissionREAD Nothing) pRow
  let pa = p{ partyAccount = Just a }
      a = ba{ accountParty = pa }
  -- Create an account with no password, and the email provided
  let _tenv_a6PRz = unknownPGTypeEnv
  dbExecute1' -- (insertAccount 'ident 'a)
   (mapQuery2
    ((\ _p_a6PRA _p_a6PRB _p_a6PRC _p_a6PRD ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (INSERT INTO account (id,email) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PRz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PRA,
                        Data.String.fromString ",",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PRz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a6PRB,
                        Data.String.fromString
                          ") RETURNING *) INSERT INTO audit.account SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PRz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PRC,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PRz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6PRD,
                        Data.String.fromString ", 'add'::audit.action, * FROM audit_row"]))
      (partyId $ partyRow (accountParty a))
      (accountEmail a)
      (auditWho ident)
      (auditIp ident))
     (\ [] -> ()))
  return a

removeParty :: MonadAudit c m => Party -> m Bool
removeParty p = do
  ident <- getAuditIdentity
  dbTransaction $ handleJust (guard . isForeignKeyViolation) (\_ -> return False) $ do
    let (_tenv_a6PXO, _tenv_a6PZT) = (unknownPGTypeEnv, unknownPGTypeEnv)
    _ <- dbExecute1 -- (deleteAccount 'ident 'p)
     (mapQuery2
      ((\ _p_a6PXP _p_a6PXQ _p_a6PXR ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (DELETE FROM account WHERE id=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PXO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PXP,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.account SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PXO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PXQ,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PXO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6PXR,
                        Data.String.fromString
                          ", 'remove'::audit.action, * FROM audit_row"]))
       (partyId $ partyRow p) (auditWho ident) (auditIp ident))
      (\[] -> ()))
    dbExecute1 -- .(deleteParty 'ident 'p)
     (mapQuery2
       ((\ _p_a6PZU _p_a6PZV _p_a6PZW ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (DELETE FROM party WHERE id=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PZT
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PZU,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.party SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PZT
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PZV,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PZT
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6PZW,
                        Data.String.fromString
                          ", 'remove'::audit.action, * FROM audit_row"]))
        (partyId $ partyRow p) (auditWho ident) (auditIp ident))
        (\[] -> ()))

lookupFixedParty :: Id Party -> Identity -> Maybe Party
lookupFixedParty (Id (-1)) _ = Just nobodyParty
lookupFixedParty (Id 0) i =
  Just rootParty{
    partyPermission = accessPermission i `max` PermissionSHARED
  , partyAccess = (accessMember i > PermissionNONE) `thenUse` view i }
lookupFixedParty i a = view a `useWhen` (i == view a)

isNobodyParty :: Party -> Bool
isNobodyParty = (0 <) . unId . partyId . partyRow

-- | Given the id for a party, ensure ... and resolve the id to the full party object. The produced party has permissions
-- for the retrieving viewer baked in.
lookupParty :: (MonadDB c m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectParty 'ident) "$WHERE party.id = ${i}")

getDuplicateParties :: MonadDB c m => m [PartyRow]
getDuplicateParties =
  dbQuery
    $(selectQuery selectPartyRow
        "$WHERE exists \
        \ (select * \
        \  from party p2 \
        \  where p2.prename = party.prename and p2.name = party.name and party.id < p2.id) ")

lookupPartyAuthorizations :: (MonadDB c m, MonadHasIdentity c m) => m [(Party, Maybe Permission)]
lookupPartyAuthorizations = do
  ident <- peek
  let _tenv_a6Qkm = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery (selectPartyAuthorization 'ident) "WHERE party.id > 0")
   (mapQuery2
                      (BS.concat
                         [Data.String.fromString
                            "SELECT party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,authorize_view.site,authorize_view.member FROM party LEFT JOIN account USING (id) LEFT JOIN authorize_view ON party.id = authorize_view.child AND authorize_view.parent = 0 WHERE party.id > 0"])
              (\
                 [_cid_a6Qkn,
                  _cname_a6Qko,
                  _cprename_a6Qkp,
                  _corcid_a6Qkq,
                  _caffiliation_a6Qkr,
                  _curl_a6Qks,
                  _cemail_a6Qkt,
                  _csite_a6Qku,
                  _cmember_a6Qkv]
                 -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cid_a6Qkn,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cname_a6Qko,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cprename_a6Qkp,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                       _corcid_a6Qkq,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _caffiliation_a6Qkr,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _curl_a6Qks,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                       _cemail_a6Qkt,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                       _csite_a6Qku,
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                       _cmember_a6Qkv)))
  pure
     (fmap
      (\ (vid_a6Qir, vname_a6Qis, vprename_a6Qit, vorcid_a6Qiu,
          vaffiliation_a6Qiv, vurl_a6Qiw, vemail_a6Qix, vsite_a6Qiz,
          vmember_a6QiA)
         -> Model.Party.SQL.makePartyAuthorization
              (Model.Party.SQL.permissionParty
                 (Model.Party.SQL.makeParty
                    (PartyRow
                       vid_a6Qir
                       vname_a6Qis
                       vprename_a6Qit
                       vorcid_a6Qiu
                       vaffiliation_a6Qiv
                       vurl_a6Qiw)
                    (do { cm_a6QiP <- vemail_a6Qix;
                          Just (Account cm_a6QiP) }))
                 Nothing
                 ident)
              (do { cm_a6QiV <- vsite_a6Qiz;
                    cm_a6QiW <- vmember_a6QiA;
                    Just (Access cm_a6QiV cm_a6QiW) }))
      rows)

-- | Find a party by id, populating the party's permission based on
-- a complicated set of cascading rules that determines the current viewer's
-- permissions over the party.
lookupAuthParty :: (MonadDB c m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupAuthParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectAuthParty 'ident) "$WHERE party.id = ${i}")

-- | resolve email to its party and enclosing account and site authenticated identity, possibly case insensitive
lookupSiteAuthByEmail
    :: MonadDB c m
    => Bool -- ^ be case-insensitive?
    -> BS.ByteString
    -> m (Maybe SiteAuth)
lookupSiteAuthByEmail caseInsensitive e = do
  let _tenv_a6QFG = unknownPGTypeEnv
  mRow <- dbQuery1 -- (selectQuery selectSiteAuth "WHERE account.email = ${e}")
    (mapQuery2
      ((\ _p_a6QFH ->
                       BS.concat
                          [Data.String.fromString
                             "SELECT party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,account.password,authorize_view.site,authorize_view.member FROM party JOIN account USING (id) LEFT JOIN authorize_view ON account.id = authorize_view.child AND authorize_view.parent = 0 WHERE account.email = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6QFG
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6QFH])
         e)
               (\
                  [_cid_a6QFI,
                   _cname_a6QFJ,
                   _cprename_a6QFK,
                   _corcid_a6QFM,
                   _caffiliation_a6QFN,
                   _curl_a6QFP,
                   _cemail_a6QFR,
                   _cpassword_a6QFT,
                   _csite_a6QFU,
                   _cmember_a6QFW]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6QFI,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a6QFJ,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a6QFK,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a6QFM,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a6QFN,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a6QFP,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cemail_a6QFR,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cpassword_a6QFT,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _csite_a6QFU,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QFG
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cmember_a6QFW)))
  let r =
        fmap
          (\ (vid_a6QyG, vname_a6QyI, vprename_a6QyJ, vorcid_a6QyL,
              vaffiliation_a6QyN, vurl_a6QyO, vemail_a6QyP, vpassword_a6QyQ,
              vsite_a6QyR, vmember_a6QyS)
             -> Model.Party.SQL.makeSiteAuth
                  (Model.Party.SQL.makeUserAccount
                     (Model.Party.SQL.makeAccount
                        (PartyRow
                           vid_a6QyG
                           vname_a6QyI
                           vprename_a6QyJ
                           vorcid_a6QyL
                           vaffiliation_a6QyN
                           vurl_a6QyO)
                        (Account vemail_a6QyP)))
                  vpassword_a6QyQ
                  (do { cm_a6Qz5 <- vsite_a6QyR;
                        cm_a6Qz6 <- vmember_a6QyS;
                        Just (Access cm_a6Qz5 cm_a6Qz6) }))
            mRow
  if caseInsensitive && isNothing r
    then do
      let _tenv_a6QN9 = unknownPGTypeEnv
      rows <- dbQuery -- (selectQuery selectSiteAuth "WHERE lower(account.email) = lower(${e}) LIMIT 2")
         (mapQuery2
           ((\ _p_a6QNa ->
                       BS.concat
                          [Data.String.fromString
                             "SELECT party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,account.password,authorize_view.site,authorize_view.member FROM party JOIN account USING (id) LEFT JOIN authorize_view ON account.id = authorize_view.child AND authorize_view.parent = 0 WHERE lower(account.email) = lower(",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6QN9
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6QNa,
                           Data.String.fromString ") LIMIT 2"])
            e)
               (\
                  [_cid_a6QNb,
                   _cname_a6QNc,
                   _cprename_a6QNd,
                   _corcid_a6QNf,
                   _caffiliation_a6QNg,
                   _curl_a6QNh,
                   _cemail_a6QNi,
                   _cpassword_a6QNj,
                   _csite_a6QNk,
                   _cmember_a6QNl]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6QNb,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a6QNc,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a6QNd,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a6QNf,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a6QNg,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a6QNh,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cemail_a6QNi,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cpassword_a6QNj,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _csite_a6QNk,
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6QN9
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cmember_a6QNl)))
      let a = fmap
                   (\ (vid_a6QLV, vname_a6QLW, vprename_a6QLX, vorcid_a6QLZ,
                       vaffiliation_a6QM0, vurl_a6QM1, vemail_a6QM2, vpassword_a6QM3,
                       vsite_a6QM4, vmember_a6QM5)
                    -> Model.Party.SQL.makeSiteAuth
                         (Model.Party.SQL.makeUserAccount
                            (Model.Party.SQL.makeAccount
                               (PartyRow
                                  vid_a6QLV
                                  vname_a6QLW
                                  vprename_a6QLX
                                  vorcid_a6QLZ
                                  vaffiliation_a6QM0
                                  vurl_a6QM1)
                               (Account vemail_a6QM2)))
                         vpassword_a6QM3
                         (do { cm_a6QMz <- vsite_a6QM4;
                               cm_a6QMA <- vmember_a6QM5;
                               Just (Access cm_a6QMz cm_a6QMA) }))
                     rows
      return $ case a of
        [x] -> Just x
        _ -> Nothing
    else
      return r

auditAccountLogin :: (MonadHasRequest c m, MonadDB c m) => Bool -> Party -> BS.ByteString -> m ()
auditAccountLogin success who email = do
  let _tenv_a6QTK = unknownPGTypeEnv
  ip <- getRemoteIp
  dbExecute1' -- [pgSQL|INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES
    -- (${if success then AuditActionOpen else AuditActionAttempt}, -1, ${ip}, ${partyId $ partyRow who}, ${email})|]
   (mapQuery2
    ((\ _p_a6QTP _p_a6QTQ _p_a6QTR _p_a6QTS ->
                    (BS.concat
                       [Data.String.fromString
                          "INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES\n\
                          \    (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6QTK
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "audit.action")
                          _p_a6QTP,
                        Data.String.fromString ", -1, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6QTK
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6QTQ,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6QTK
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6QTR,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6QTK
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a6QTS,
                        Data.String.fromString ")"]))
      (if success then AuditActionOpen else AuditActionAttempt)
      ip
      (partyId $ partyRow who)
      email)
     (\[] -> ()))

recentAccountLogins :: MonadDB c m => Party -> m Int64
recentAccountLogins who = fromMaybe 0 <$>
  dbQuery1 -- [pgSQL|!SELECT count(*) FROM audit.account WHERE audit_action = 'attempt' AND id = ${partyId $ partyRow who} AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'|]
    (let _tenv_a6QXO = unknownPGTypeEnv
     in
       mapQuery2
        ((\ _p_a6QXP ->
                    (BS.concat
                       [Data.String.fromString
                          "SELECT count(*) FROM audit.account WHERE audit_action = 'attempt' AND id = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6QXO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6QXP,
                        Data.String.fromString
                          " AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'"]))
         (partyId $ partyRow who))
            (\ [_ccount_a6QXQ]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a6QXO
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                     _ccount_a6QXQ)))

-- | Filter criteria and result paging options
data PartyFilter = PartyFilter
  { partyFilterQuery :: Maybe String
    -- ^ pattern to compare first name, last name, and possibly email
  , partyFilterAuthorization :: Maybe Permission
    -- ^ match on this permission level in accessing the databrary site group's data
  , partyFilterInstitution :: Maybe Bool
    -- ^ either only include institutions (True) or
    -- only include human parties with active account (False)
  , partyFilterPaginate :: Paginate
  }

instance Monoid PartyFilter where
  mempty = PartyFilter Nothing Nothing Nothing def
  mappend (PartyFilter q1 a1 i1 p) (PartyFilter q2 a2 i2 _) =
    PartyFilter (q1 <> q2) (a1 <|> a2) (i1 <|> i2) p

partyFilter :: PartyFilter -> Identity -> BS.ByteString
partyFilter PartyFilter{..} ident = BS.concat
  [ withq partyFilterAuthorization (const " JOIN authorize_view ON party.id = child AND parent = 0")
  , " WHERE id > 0 AND id != ", pgLiteralRep (partyId $ partyRow staffParty)
  , withq partyFilterQuery (\n -> " AND " <> queryVal <> " ILIKE " <> pgLiteralRep (wordPat n))
  , withq partyFilterAuthorization (\a -> " AND site = " <> pgSafeLiteral a)
  , withq partyFilterInstitution (\i -> if i then " AND account.id IS NULL" else " AND account.password IS NOT NULL")
  , " ORDER BY name, prename "
  , paginateSQL partyFilterPaginate
  ]
  where
  withq v f = maybe "" f v
  wordPat = intercalate "%" . ("":) . (++[""]) . words
  queryVal
    | showEmail ident = "(COALESCE(prename || ' ', '') || name || COALESCE(' ' || email, ''))"
    | otherwise = "(COALESCE(prename || ' ', '') || name)"

findParties :: (MonadHasIdentity c m, MonadDB c m) => PartyFilter -> m [Party]
findParties pf = do
  let _tenv_a6R7j = unknownPGTypeEnv
  ident <- peek
  rows <- dbQuery $ unsafeModifyQuery -- (selectQuery (selectParty 'ident) "")
    (mapQuery2
       (BS.concat
             -- TODO: this duplicates logic in lookupAuthorization slightly
            [Data.String.fromString
                "SELECT \
                \  party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email \
                \ ,COALESCE(av.site, 'NONE') \
                \ FROM party \
                \   LEFT JOIN account USING (id) \
                \   LEFT JOIN authorize_view av \
                \      ON party.id = av.child AND av.parent = 0 "])
        (\
           [_cid_a6R7m,
            _cname_a6R7o,
            _cprename_a6R7p,
            _corcid_a6R7q,
            _caffiliation_a6R7r,
            _curl_a6R7s,
            _cemail_a6R7t,
            site]
           -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                 _cid_a6R7m,
               Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "text")
                 _cname_a6R7o,
               Database.PostgreSQL.Typed.Types.pgDecodeColumn
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "text")
                 _cprename_a6R7p,
               Database.PostgreSQL.Typed.Types.pgDecodeColumn
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                 _corcid_a6R7q,
               Database.PostgreSQL.Typed.Types.pgDecodeColumn
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "text")
                 _caffiliation_a6R7r,
               Database.PostgreSQL.Typed.Types.pgDecodeColumn
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "text")
                 _curl_a6R7s,
               Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                 _cemail_a6R7t,
               Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                 _tenv_a6R7j
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                 site)))
    (<> partyFilter pf ident)
  pure
    (fmap
      (\ (vid_a6R3R, vname_a6R3S, vprename_a6R3T, vorcid_a6R3U,
          vaffiliation_a6R3V, vurl_a6R3W, vemail_a6R3X, site)
         -> Model.Party.SQL.permissionParty
              (Model.Party.SQL.makeParty2
                 (PartyRow
                    vid_a6R3R
                    vname_a6R3S
                    vprename_a6R3T
                    vorcid_a6R3U
                    vaffiliation_a6R3V
                    vurl_a6R3W)
                 (do { cm_a6R44 <- vemail_a6R3X;
                       Just (Account cm_a6R44) })
                 (Loaded site))
              Nothing
              ident)
      rows)

lookupAvatar :: MonadDB c m => Id Party -> m (Maybe Asset)
lookupAvatar p =
  dbQuery1 $ (`Asset` coreVolume) <$> $(selectQuery selectAssetRow $ "$JOIN avatar ON asset.id = avatar.asset WHERE avatar.party = ${p} AND asset.volume = " ++ pgLiteralString (volumeId $ volumeRow coreVolume))

changeAvatar :: MonadAudit c m => Party -> Maybe Asset -> m Bool
changeAvatar p Nothing = do
  let _tenv_a76io = unknownPGTypeEnv
  ident <- getAuditIdentity
  dbExecute1 -- (auditDelete 'ident "avatar" "party = ${partyId $ partyRow p}" Nothing)
   (mapQuery2
    ((\ _p_a76ip _p_a76iq _p_a76ir ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (DELETE FROM avatar WHERE party = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76io
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76ip,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.avatar SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76io
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76iq,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76io
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a76ir,
                        Data.String.fromString
                          ", 'remove'::audit.action, * FROM audit_row"]))
      (partyId $ partyRow p) (auditWho ident) (auditIp ident))
            (\[] -> ()))
changeAvatar p (Just a) = do
  let (_tenv_a76iP, _tenv_a76jh) = (unknownPGTypeEnv, unknownPGTypeEnv)
  ident <- getAuditIdentity
  (0 <) . fst <$> updateOrInsert
    -- (auditUpdate 'ident "avatar" [("asset", "${assetId $ assetRow a}")] "party = ${partyId $ partyRow p}" Nothing)
    (mapQuery2
      ((\ _p_a76iQ _p_a76iR _p_a76iS _p_a76iT ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE avatar SET asset=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76iP
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76iQ,
                        Data.String.fromString " WHERE party = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76iP
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76iR,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.avatar SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76iP
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76iS,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76iP
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a76iT,
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"]))
        (assetId $ assetRow a)
        (partyId $ partyRow p)
        (auditWho ident)
        (auditIp ident))
            (\ [] -> ()))
    -- (auditInsert 'ident "avatar" [("asset", "${assetId $ assetRow a}"), ("party", "${partyId $ partyRow p}")] Nothing)
    (mapQuery2
     ((\ _p_a76ji _p_a76jj _p_a76jk _p_a76jl ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (INSERT INTO avatar (asset,party) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76jh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76ji,
                        Data.String.fromString ",",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76jh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76jj,
                        Data.String.fromString
                          ") RETURNING *) INSERT INTO audit.avatar SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76jh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a76jk,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a76jh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a76jl,
                        Data.String.fromString ", 'add'::audit.action, * FROM audit_row"]))
          (assetId $ assetRow a)
          (partyId $ partyRow p)
          (auditWho ident)
          (auditIp ident))
       (\ [] -> ()))

