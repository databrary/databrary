module Databrary.View.Html
  ( lazyByteStringHtml
  , byteStringHtml
  , builderHtml
  , lazyByteStringValue
  , byteStringValue
  , builderValue
  , actionLink
  , actionForm
  , (!?)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Blaze.Internal as Markup
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Blaze.ByteString.Builder.Html.Word (fromHtmlEscapedByteString, fromHtmlEscapedLazyByteString)
import Databrary.Action
import Databrary.HTTP.Route

lazyByteStringHtml :: BSL.ByteString -> H.Markup
lazyByteStringHtml = H.unsafeLazyByteString . BSB.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringHtml :: BS.ByteString -> H.Markup
byteStringHtml = H.unsafeLazyByteString . BSB.toLazyByteString . fromHtmlEscapedByteString

builderHtml :: BSB.Builder -> H.Markup
builderHtml = lazyByteStringHtml . BSB.toLazyByteString

lazyByteStringValue :: BSL.ByteString -> H.AttributeValue
lazyByteStringValue = H.unsafeLazyByteStringValue . BSB.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringValue :: BS.ByteString -> H.AttributeValue
byteStringValue = H.unsafeLazyByteStringValue . BSB.toLazyByteString . fromHtmlEscapedByteString

builderValue :: BSB.Builder -> H.AttributeValue
builderValue = lazyByteStringValue . BSB.toLazyByteString

actionLink :: Route a r -> a -> H.Attribute
actionLink r a = HA.href $ byteStringValue $ actionURL r a Nothing

actionForm :: Route a r -> a -> H.Html -> H.Html
actionForm r@Route{ routeMethod = g, routeMultipart = p } a = H.form
  H.! HA.method (H.unsafeByteStringValue g)
  H.!? (p, HA.enctype $ H.toValue "multipart/form-data")
  H.! HA.action (builderValue $ renderRoute r a)

(!?) :: Markup.Attributable h => h -> Maybe H.Attribute -> h
h !? Nothing = h
h !? (Just a) = h H.! a
