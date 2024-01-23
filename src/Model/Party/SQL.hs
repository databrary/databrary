{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Model.Party.SQL
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
  , makeParty2
  ) where

import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Monoid ((<>))
import qualified Language.Haskell.TH as TH

import Has (Has, view)
import Model.SQL.Select
import Model.Audit.SQL
import Model.Permission.Types
import Model.Permission.SQL
import Model.Id.Types
import Model.Identity.Types
import Model.Party.Types

selectPartyRow :: Selector -- ^ @'PartyRow'@
selectPartyRow = selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]

accountRow :: Selector -- ^ @'Party' -> 'Account'@
accountRow = selectColumns 'Account "account" ["email"]

-- | Build party, with a circular connection to an account if an account creation function is provided
makeParty :: PartyRow -> Maybe (Party -> Account) -> Permission -> Maybe Access -> Party
makeParty pr mMkAcct perm mAccess = makeParty2 pr mMkAcct NotLoaded perm mAccess

-- | Build party, with a circular connection to an account if an account creation function is provided.
-- Transition function taking mSiteAccess until all queries use this.
makeParty2 :: PartyRow -> Maybe (Party -> Account) -> Loaded Permission -> Permission -> Maybe Access -> Party
makeParty2 pr mMkAcct lSiteAccess perm mAccess =
    p
  where
    p = Party pr (fmap (\mkAcct -> mkAcct p) mMkAcct) lSiteAccess perm mAccess

selectPermissionParty :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Party'@
selectPermissionParty = selectJoin 'makeParty
  [ selectPartyRow
  , maybeJoinUsing ["id"] accountRow
  ]

-- | Build an account or party, based on calling context.
-- Compute permission and access by coalescing authorization granted directly and generally
permissionParty
  :: Has (Id Party) a
  => (Permission -> Maybe Access -> a) -- ^ Partially applied makeParty, ready to build full account or party
  -> Maybe Access -- ^ The direct authorization that the party/account being built may have authorized to the
                  -- viewing identity/user. This is only used by lookupAuthParty, which is only used in the
                  -- context of retrieving a party for editing/viewing in isolation by the party controller actions
  -> Identity -- ^ The viewing identity / user which is trying to view or edit the party being retrieved.
  -> a -- ^ account or party
permissionParty mkPartyOrAcct mGrantedAccessFromPartyToViewer viewingIdent =
    p
  where
    p = mkPartyOrAcct maxPermission mAccessDeduced
    maxPermission :: Permission
    maxPermission =
        case mAccessDeduced of
            -- if there is no Identity associated Access, then use the viewing actors bounded permission
            Nothing -> maxDefaultDerivedFromActor
            -- if there is an Identity Access, then max with identity's lowest access perm
            Just accessDeduced -> max (accessPermission' accessDeduced) maxDefaultDerivedFromActor
    -- | Push the viewingIdent's site access permission to the highest value within Public ... Read
    -- This default value is derived from the viewingIdent's granted databrary wide site access.
    maxDefaultDerivedFromActor :: Permission
    maxDefaultDerivedFromActor =
        max PermissionPUBLIC  -- then, lower bound with public
          $ min PermissionREAD  -- upper bound with read (just because you have higher privileges on site doesn't mean
                                -- you can edit any party's data)
            -- accessSite means extract Access from the identity, then extract site field of that Access record
            -- for NotLoggedIn and IdentityNotNeeded, the access is (None,None) via nobodySiteAuth
            -- for Identified, the access is the levels granted to the user on the databrary site via it's parent
            $ accessSite' generalSiteAccessForViewer
    mAccessDeduced :: Maybe Access
    mAccessDeduced
      -- if the viewing identity is Identified, and the viewer is the same as the party being retrieved,
      --  then allow unbounded permission on the retrieved party (self)
      | extractFromIdentifiedSessOrDefault
              False
              (\viewingSess -> (view p :: Id Party) == (view viewingSess :: Id Party))
              viewingIdent =
            Just maxBound
      -- if the viewing user is a sitewide admin
      | identityAdmin viewingIdent =
            Just
                (case mGrantedAccessFromPartyToViewer of
                    Nothing -> generalSiteAccessForViewer -- get access via siteauth
                     -- max elements between granted access and access via siteauth
                    Just granted -> granted <> generalSiteAccessForViewer)
      -- the viewing user is normal and someone isn't trying to edit/view themselves
      | otherwise =
            mGrantedAccessFromPartyToViewer
    generalSiteAccessForViewer :: Access
    generalSiteAccessForViewer = view viewingIdent

selectParty :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Party'@
selectParty ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` TH.ConE 'Nothing) . (TH.VarE 'permissionParty `TH.AppE`))
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

-- | Used by 'makeUserAccount' and 'selectPermissionAccount'. This finishes building the circular Party and Account structure.
makeAccount :: PartyRow -> (Party -> Account) -> Permission -> Maybe Access -> Account
makeAccount pr ac perm ma = a where -- TODO: receive Loaded Permission for site authorization value
  a = ac $ Party pr (Just a) NotLoaded perm ma

selectPermissionAccount :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Account'@
selectPermissionAccount = selectJoin 'makeAccount
  [ selectPartyRow
  , joinUsing ["id"] accountRow
  ]

selectAccount :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Account'@
selectAccount ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` TH.ConE 'Nothing) . (TH.VarE 'permissionParty `TH.AppE`))
  selectPermissionAccount

-- | Build an account, using ADMIN permission and (ADMIN,ADMIN) access object.
-- Essentially a user has full permissions over themselves and the access object
-- nested within the account party has max privileges.
-- I suspect that the access object within a Party, when nested inside of a SiteAuth
-- is almost never used, as the Has instances all retrieve siteAccess, not (partyAccess . accountParty . siteAccount)
makeUserAccount :: (Permission -> Maybe Access -> Account) -> Account
makeUserAccount mkAcc = mkAcc maxBound (Just maxBound)

selectUserAccount :: Selector -- @'Account'
selectUserAccount = selectMap (TH.VarE 'makeUserAccount `TH.AppE`) selectPermissionAccount

-- | Build a SiteAuth object using the provided Account, possible password, and possible access object.
-- If no access object is provided, then use (None, None) for the access object.
makeSiteAuth :: Account -> Maybe BS.ByteString -> Maybe Access -> SiteAuth
makeSiteAuth account mPassword mAccess = SiteAuth account mPassword (fold mAccess)

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
