{-# LANGUAGE OverloadedStrings #-}
module View.Template
  ( htmlHeader
  , htmlFooter
  , htmlTemplate
  , htmlSocialMedia
  ) where

import Control.Monad (void, when, forM_)
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Network.HTTP.Types (methodGet)
import qualified Network.Wai as Wai

import Paths_databrary (version)
import Ops
import Has (view)
import Model.Identity
import Action.Types
import Action.Route
import Controller.Paths
import View.Html

import {-# SOURCE #-} Controller.Angular
import {-# SOURCE #-} Controller.Root
import {-# SOURCE #-} Controller.Login
import {-# SOURCE #-} Controller.Party
import {-# SOURCE #-} Controller.Web

htmlHeader :: Maybe BSB.Builder -> JSOpt -> H.Html
htmlHeader canon hasjs = do
  forM_ canon $ \c ->
    H.link
      H.! HA.rel "canonical"
      H.! HA.href (builderValue c)
  H.link
    H.! HA.rel "shortcut icon"
    H.! HA.href (builderValue $ actionURL Nothing webFile (Just $ staticPath ["icons", "favicon.png"]) [])
  H.link
    H.! HA.rel "start"
    H.! actionLink viewRoot HTML hasjs
  -- forM_ ["news", "about", "access", "community"] $ \l -> H.link
  forM_ ["about", "support"] $ \l -> H.link
    H.! HA.rel l
    H.! HA.href ("//databrary.org/" <> l <> ".html")

htmlAddress :: H.Html
htmlAddress =
  H.p H.! HA.class_ "footer-address" $ do
    H.strong $
      void "Databrary"
    H.br
    void "4 Washington Place, Room 409 | New York, NY 10003"
    H.br
    void "212.998.5800"

htmlCopyrightTermsPrivacy :: H.Html
htmlCopyrightTermsPrivacy =
  H.p H.! HA.class_ "footer-copyright" $ do
    void "Copyright © 2014-2023"
    void " | "
    H.a H.! HA.href "https://databrary.org/about/policies/terms.html" $ "Terms of Use"
    void " | "
    H.a H.! HA.href "https://databrary.org/about/policies/privacy.html" $ "Privacy Policy"
    H.br

htmlSocialMedia :: H.Html
htmlSocialMedia =
  H.p H.! HA.class_ "footer-social-media" $ do
    let sm n l a =
          H.a H.! HA.href l H.! HA.target "_blank" H.! HA.class_ "img" $
            H.img H.! HA.id n H.! HA.src ("/web/images/social/16px/" <> n <> ".png") H.! HA.alt a
    void "Find us on "
    sm "twitter" "https://twitter.com/databrary" "Twitter"
    void " "
    sm "facebook" "https://www.facebook.com/databrary" "Facebook"
    void " "
    sm "linkedin" "https://www.linkedin.com/company/databrary-project" "LinkedIn"
    void " "
    -- sm "google-plus" "https://plus.google.com/u/1/111083162045777800330/posts" "Google+"
    -- void " "
    sm "github" "https://github.com/databrary/" "GitHub"

htmlFooter :: H.Html
htmlFooter = H.footer H.! HA.id "site-footer" H.! HA.class_ "site-footer" $
  H.div H.! HA.class_ "wrap" $
    H.div H.! HA.class_ "row" $ do
      H.div H.! HA.class_ "site-footer-social-address" $ do
        -- htmlAddress
        htmlCopyrightTermsPrivacy
        htmlSocialMedia
      H.ul H.! HA.class_ "site-footer-grants" $ do
        H.li $
          H.a H.! HA.href "http://www.nyu.edu" $
            H.img H.! HA.src "/web/images/grants/nyu-small.jpg" H.! HA.class_ "nyu" H.! HA.alt "funder NYU logo"
        H.li $
          H.a H.! HA.href "http://www.psu.edu" $
            H.img H.! HA.src "/web/images/grants/pennstate.png" H.! HA.class_ "psu" H.! HA.alt "funder Penn State logo"
        H.li $
          H.a H.! HA.href "http://www.nsf.gov/awardsearch/showAward?AWD_ID=1238599&HistoricalAwards=false" $ do
            H.img H.! HA.src "/web/images/grants/nsf.png" H.! HA.class_ "nsf" H.! HA.alt "funder NSF logo"
            " BCS-1238599"
        H.li $
          H.a H.! HA.href "https://www.nsf.gov/awardsearch/showAward?AWD_ID=2032713&HistoricalAwards=false" $ do
            H.img H.! HA.src "/web/images/grants/nsf.png" H.! HA.class_ "nsf" H.! HA.alt "funder NSF logo"
            " OAC-2032713"
        H.li $
          H.a H.! HA.href "http://projectreporter.nih.gov/project_info_description.cfm?aid=8531595&icde=15908155&ddparam=&ddvalue=&ddsub=&cr=1&csb=default&cs=ASC" $ do
            H.img H.! HA.src "/web/images/grants/nih.png" H.! HA.class_ "nih" H.! HA.alt "funder NICHD logo"
            " U01-HD-076595"
        H.li $
          H.a H.! HA.href "https://www.srcd.org/" $
            H.img H.! HA.src "/web/images/grants/srcd.png" H.! HA.class_ "srcd" H.! HA.alt "funder SRCD logo"
        H.li $
          H.a H.! HA.href "https://sloan.org/" $
            H.img H.! HA.src "/web/images/grants/sloan.png" H.! HA.class_ "sloan" H.! HA.alt "funder Sloan logo"
        H.li $
          H.a H.! HA.href "https://www.legofoundation.com" $
            H.img H.! HA.src "/web/images/grants/lego.png" H.! HA.class_ "lego" H.! HA.alt "funder Lego logo"
      H.div H.! HA.class_ "site-footer-legal col" $
        H.p $ do
          void "Each dataset on Databrary represents an individual work owned by the party who contributed it. Use of Databrary is subject to the "
          H.a H.! HA.href "https://databrary.org/about/policies/terms.html" H.! HA.target "_blank" $
            "Terms & Conditions of Use" 
          void " and the "
          H.a H.! HA.href "https://databrary.org/about/agreement/agreement.html" H.! HA.target "_blank" $
            "Databrary Access Agreement."           
          -- H.string $ showVersion version
          -- "]"

htmlTemplate :: RequestContext -> Maybe T.Text -> (JSOpt -> H.Html) -> H.Html
htmlTemplate req title body = H.docTypeHtml $ do
  H.head $ do
    htmlHeader canon hasjs
    H.link
      H.! HA.rel "stylesheet"
      H.! actionLink webFile (Just $ StaticPath "all.min.css") ([] :: Query)
    H.title $ do
      mapM_ (\t -> H.toHtml t >> " || ") title
      "Databrary"
  H.body H.! H.customAttribute "vocab" "http://schema.org" $ do
    H.section
      H.! HA.id "toolbar"
      H.! HA.class_ "toolbar"
      $ H.div
        H.! HA.class_ "wrap toolbar-main"
        $ H.div
          H.! HA.class_ "row"
          $ H.nav
            H.! HA.class_ "toolbar-nav no-angular cf"
            $ do
              H.ul
                H.! HA.class_ "inline-block flat cf"
                $ do
                  H.li $ H.a
                    H.! actionLink viewRoot HTML hasjs
                    $ "Databrary"
                  -- forM_ ["news", "about", "access", "community"] $ \l ->
                  forM_ ["about", "support"] $ \l ->                    
                    H.li $ H.a H.! HA.href (H.stringValue $ "//databrary.org/" ++ l ++ ".html") $
                      H.string l
              H.ul
                H.! HA.class_ "toolbar-user inline-block flat cf"
                $ extractFromIdentifiedSessOrDefault
                  (H.li $ H.a H.! actionLink viewLogin () hasjs $ "Login")
                  (\_ -> do
                    H.li $ H.a H.! actionLink viewParty (HTML, TargetProfile) hasjs $ "Your Dashboard"
                    H.li $ actionForm postLogout HTML hasjs $
                      H.button
                        H.! HA.class_ "mini"
                        H.! HA.type_ "submit"
                        $ "Logout")
                  $ requestIdentity req
    H.section
      H.! HA.id "main"
      H.! HA.class_ "main"
        $ H.div
          H.! HA.class_ "wrap"
          $ H.div
            H.! HA.class_ "row"
            $ do
              when (hasjs /= JSEnabled) $ forM_ canon $ \c -> H.div $ do
                H.preEscapedString "Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others). \
                  \You are viewing the simple version of our site: some functionality may not be available. \
                  \Try switching to the "
                H.a H.! HA.href (builderValue c) $ "modern version"
                " to see if it will work on your browser."
              mapM_ (H.h1 . H.toHtml) title
                H.! HA.class_ "view-title"
              r <- body hasjs
              htmlFooter
              return r
  where
  -- FIXME: I don't think these lines do what they think they do.
  (hasjs, nojs) = jsURL JSDefault (view req)
  canon = (Wai.requestMethod (view req) == methodGet && hasjs == JSDefault) `unlessUse` nojs
