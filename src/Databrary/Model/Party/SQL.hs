{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Party.SQL
  ( selectPartyRow
  , selectParty
  , selectPartyAuthorization
  , selectAuthParty
  , selectAccount
  , selectUserAccount
  , selectSiteAuth
  , updateParty
  , updateAccount
  , insertParty
  , insertAccount
  , deleteParty
  , deleteAccount
  -- for expanded queries
  , makeSiteAuth
  , makeUserAccount
  , makeAccount
  , makePartyAuthorization
  , permissionParty
  , makeParty
  ) where

import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Monoid ((<>))
import qualified Language.Haskell.TH as TH

import Databrary.Has (Has, view)
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Permission.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types

selectPartyRow :: Selector -- ^ @'PartyRow'@
selectPartyRow = selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]

accountRow :: Selector -- ^ @'Party' -> 'Account'@
accountRow = selectColumns 'Account "account" ["email"]

-- | Build party, with a circular connection to an account if an account creation function is provided
makeParty :: PartyRow -> Maybe (Party -> Account) -> Permission -> Maybe Access -> Party
makeParty pr mMkAcct perm mAccess =
    p
  where
    p = Party pr (fmap (\mkAcct -> mkAcct p) mMkAcct) perm mAccess

selectPermissionParty :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Party'@
selectPermissionParty = selectJoin 'makeParty
  [ selectPartyRow
  , maybeJoinUsing ["id"] accountRow
  ]

-- | Build an account or party, based on calling context.
permissionParty
  :: Has (Id Party) a
  => (Permission -> Maybe Access -> a) -- ^ Partially applied makeParty, ready to build full account or party
  -> Maybe Access -- ^ The direct authorization that the party/account being built may have authorized to the
                  -- viewing identity/user. This is only used by lookupAuthParty, which is only used in the
                  -- context of retreiving a party for editing/viewing in isolation by the party controller actions
  -> Identity -- ^ The viewing identity / user which is trying to view or edit the party being retrieved.
  -> a -- ^ account or party
permissionParty mkPartyOrAcct access1 viewingIdent =
    p
  where
    p =
      mkPartyOrAcct
        (combineWithAccessPermissions mAccessDeduced boundedPermFromActor)
        mAccessDeduced
    combineWithAccessPermissions :: Maybe Access -> (Permission -> Permission)
    combineWithAccessPermissions mAccess =
        maybe
          id  -- if there is no Identity associated Access, then use the viewing actors bounded permission
          (max . accessPermission') -- if there is an Identity Access, then max with identity's lowest access perm
          mAccess
    boundedPermFromActor :: Permission
    boundedPermFromActor = -- ends up between public ... read
        max PermissionPUBLIC  -- lower bound with public
          $ min PermissionREAD  -- upper bound with read
            $ accessSite viewingIdent
    mAccessDeduced :: Maybe Access
    mAccessDeduced
      | foldIdentity False (((view p :: Id Party) ==) . view) viewingIdent = Just maxBound
      | identityAdmin viewingIdent = Just $ maybe id (<>) access1 $ view viewingIdent
      | otherwise = access1

selectParty :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Party'@
selectParty ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` (TH.ConE 'Nothing)) . (TH.VarE 'permissionParty `TH.AppE`)) $
  selectPermissionParty

makePartyAuthorization :: Party -> Maybe Access -> (Party, Maybe Permission)
makePartyAuthorization p a = (p, accessSite <$> a)

selectPartyAuthorization :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @('Party', Maybe 'Permission')@
selectPartyAuthorization ident = selectJoin 'makePartyAuthorization
  [ selectParty ident
  , maybeJoinOn "party.id = authorize_view.child AND authorize_view.parent = 0"
    $ accessRow "authorize_view"
  ]

selectAuthParty :: TH.Name -- ^ 'Identity`
  -> Selector -- ^ @'Party'@
selectAuthParty ident = selectMap (`TH.AppE` TH.VarE ident) $ selectJoin 'permissionParty
  [ selectPermissionParty
  , maybeJoinOn ("party.id = authorize_valid.parent AND authorize_valid.child = ${view " ++ nameRef ident ++ " :: Id Party}")
    $ accessRow "authorize_valid" -- optimization, should be authorize_view if we used site
  ]

-- | Used by 'makeUserAccount' and 'selectPermissionAccount'
makeAccount :: PartyRow -> (Party -> Account) -> Permission -> Maybe Access -> Account
makeAccount pr ac perm ma = a where
  a = ac $ Party pr (Just a) perm ma

selectPermissionAccount :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Account'@
selectPermissionAccount = selectJoin 'makeAccount
  [ selectPartyRow
  , joinUsing ["id"] accountRow
  ]

selectAccount :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Account'@
selectAccount ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` (TH.ConE 'Nothing)) . (TH.VarE 'permissionParty `TH.AppE`)) $
  selectPermissionAccount

makeUserAccount :: (Permission -> Maybe Access -> Account) -> Account
makeUserAccount mkAcc = mkAcc maxBound (Just maxBound)

selectUserAccount :: Selector -- @'Account'
selectUserAccount = selectMap (TH.VarE 'makeUserAccount `TH.AppE`) selectPermissionAccount

makeSiteAuth :: Account -> Maybe BS.ByteString -> Maybe Access -> SiteAuth
makeSiteAuth p w a = SiteAuth p w (fold a)

selectSiteAuth :: Selector -- @'SiteAuth'@
selectSiteAuth = selectJoin 'makeSiteAuth
  [ selectUserAccount
  , Selector (SelectColumn "account" "password") "" ""
  , maybeJoinOn "account.id = authorize_view.child AND authorize_view.parent = 0"
    $ accessRow "authorize_view"
  ]

partyKeys :: String -- ^ @'Party'@
  -> [(String, String)]
partyKeys p =
  [ ("id", "${partyId $ partyRow " ++ p ++ "}") ]

accountKeys :: String -- ^ @'Account'@
  -> [(String, String)]
accountKeys a = partyKeys $ "(accountParty " ++ a ++ ")"

partySets :: String -- ^ @'Party'@
  -> [(String, String)]
partySets p =
  [ ("name",        "${partySortName $ partyRow "    ++ p ++ "}")
  , ("prename",     "${partyPreName $ partyRow "     ++ p ++ "}")
  , ("affiliation", "${partyAffiliation $ partyRow " ++ p ++ "}")
  , ("url",         "${partyURL $ partyRow "         ++ p ++ "}")
  ]

accountSets :: String -- ^ @'Account'@
  -> [(String, String)]
accountSets a =
  [ ("email", "${accountEmail " ++ a ++ "}")
  ]

updateParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ()
updateParty ident p = auditUpdate ident "party"
  (partySets ps)
  (whereEq $ partyKeys ps)
  Nothing
  where ps = nameRef p

updateAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Account'@
  -> TH.ExpQ -- ()
updateAccount ident a = auditUpdate ident "account"
  (accountSets as ++ [("password", "${accountPasswd " ++ us ++ "}")])
  (whereEq $ accountKeys as)
  Nothing
  where
  as = "(siteAccount " ++ us ++ ")"
  us = nameRef a

insertParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @'PartyRow'@
insertParty ident p = auditInsert ident "party"
  (partySets ps)
  (Just $ selectOutput selectPartyRow)
  where ps = nameRef p

insertAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Account'@
  -> TH.ExpQ
insertAccount ident a = auditInsert ident "account"
  (accountKeys as ++ accountSets as)
  Nothing
  where as = nameRef a

deleteParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @()@
deleteParty ident p = auditDelete ident "party"
  (whereEq $ partyKeys (nameRef p))
  Nothing

deleteAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @()@
deleteAccount ident p = auditDelete ident "account"
  (whereEq $ partyKeys (nameRef p))
  Nothing
