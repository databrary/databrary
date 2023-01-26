{-# LANGUAGE CPP, OverloadedStrings #-}
module View.Angular
  ( htmlAngular
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Default.Class (def)
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Has (view)
import qualified JSON
import Service.Types
import Model.Identity
import Action.Types
import Web (WebFilePath (..))
import Controller.Web
import View.Html
import View.Template

ngAttribute :: String -> H.AttributeValue -> H.Attribute
ngAttribute = H.customAttribute . H.stringTag . ("ng-" <>)

webURL :: BS.ByteString -> H.AttributeValue -- TODO: stop using this?
webURL p = actionValue webFile (Just $ StaticPath p) ([] :: Query)

versionedWebURL :: BS.ByteString -> BS.ByteString -> H.AttributeValue
versionedWebURL version p = actionValue webFile (Just $ StaticPath p) ([(version,Nothing)] :: Query)

htmlAngular :: BS.ByteString -> [WebFilePath] -> [WebFilePath] -> BSB.Builder -> RequestContext -> H.Html
htmlAngular assetsVersion cssDeps jsDeps nojs reqCtx = H.docTypeHtml H.! ngAttribute "app" "databraryModule" H.! H.customAttribute "lang" "en" $ do
  H.head $ do
    htmlHeader Nothing def
    H.noscript $
      H.meta
        H.! HA.httpEquiv "Refresh"
        H.! HA.content (builderValue $ BSB.string8 "0;url=" <> nojs)
    H.meta
      H.! HA.httpEquiv "X-UA-Compatible"
      H.! HA.content "IE=edge"
    H.meta
      H.! HA.name "viewport"
      H.! HA.content "width=device-width, initial-scale=1.0, minimum-scale=1.0"
    H.title
      H.! ngAttribute "bind" (byteStringValue $ "page.display.title + ' || " <> title <> "'")
      $ H.unsafeByteString title
    forM_ [Just "114x114", Just "72x72", Nothing] $ \size ->
      H.link
        H.! HA.rel "apple-touch-icon-precomposed"
        H.! HA.href (webURL $ "icons/apple-touch-icon" <> maybe "" (BSC.cons '-') size <> ".png")
        !? (HA.sizes . byteStringValue <$> size)
    forM_ cssDeps $ \css ->
      H.link
        H.! HA.rel "stylesheet"
        H.! HA.href (versionedWebURL assetsVersion $ webFileRel css)
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.href "https://allfont.net/cache/css/lucida-sans-unicode.css"
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.href "https://fonts.googleapis.com/css?family=Questrial"
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.href "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"
      H.! H.customAttribute "integrity" "sha512-xodZBNTC5n17Xt2atTPuE1HxjVMSvLVW9ocqUKLsCC5CXdbqCmblAshOMAS6/keqq/sMZMZ19scR4PsZChSR7A=="
      H.! H.customAttribute "crossorigin" ""
    H.script $
      H.preEscapedString "(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0], j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src= 'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f); })(window,document,'script','dataLayer','GTM-NW6PSFL');"
    H.script $ do
      H.preEscapedString "window.$play={user:"
      unsafeBuilder $ JSON.fromEncoding $ JSON.recordEncoding $ identityJSON (view reqCtx)
      forM_ (serviceDown (view reqCtx)) $ \msg -> do
        H.preEscapedString ",down:"
        H.unsafeLazyByteString $ JSON.encode msg
      H.preEscapedString "};"
    forM_ jsDeps $ \js ->
      H.script
        H.! HA.src (versionedWebURL assetsVersion $ webFileRel js)
        $ return ()
  H.body
    H.! H.customAttribute "flow-prevent-drop" mempty
    $ do
    H.noscript $ do
      H.preEscapedString "Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others) with Javascript enabled.  You can also switch to the "
      H.a
        H.! HA.href (builderValue nojs)
        $ "simple version"
      H.preEscapedString " of this page."
    H.preEscapedString "<toolbar></toolbar>"
    H.script H.! HA.src "https://unpkg.com/leaflet@1.7.1/dist/leaflet.js" H.! H.customAttribute "integrity" "sha512-XQoYMqMTK8LvdxXYG3nZ448hOEQiglfqkJs1NOQV44cWnUrBc8PkAOcXy20w0vlaXaVUearIOBhiXZ5V3ynxwA==" H.! H.customAttribute  "crossorigin" "" 
      $ return ()
    H.script H.! HA.src "https://cdn.statically.io/gh/leaflet-extras/leaflet-providers/d5dc3dec/leaflet-providers.js" 
      $ return ()
    H.script H.! HA.src "https://cdn.statically.io/gh/gilmore-lab/databrary-analytics/96ce5a60/institutions-investigators/js/institutions.js"
      $ return ()
    H.preEscapedString $ "<main ng-view id=\"main\" class=\"main"
#ifdef SANDBOX
      <> " sandbox"
#endif
      <> "\" autoscroll ng-if=\"!page.display.error\"></main>"
    H.preEscapedString "<errors></errors>"
    htmlFooter
    H.preEscapedString "<messages></messages>"
    H.preEscapedString "<tooltip ng-repeat=\"tooltip in page.tooltips.list\"></tooltip>"
    H.div
      H.! HA.id "loading"
      H.! HA.class_ "loading"
      H.! HA.style "display:none"
      H.! ngAttribute "show" "page.display.loading" $
      H.div H.! HA.class_ "loading-animation" $ do
        H.div H.! HA.class_ "loading-spinner" $
          H.div H.! HA.class_ "loading-mask" $
            H.div H.! HA.class_ "loading-circle" $
              return ()
        H.div H.! HA.class_ "loading-text" $
          "[" >> H.span "loading" >> "]"
    H.script
      $ H.preEscapedString "document.getElementById('loading').style.display='block';"
  where
  title =
#ifdef SANDBOX
    "Databrary Demo"
#else
    "Databrary"
#endif
