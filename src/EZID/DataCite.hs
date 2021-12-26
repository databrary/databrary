{-# LANGUAGE RecordWildCards #-}
module EZID.DataCite
  ( DataCite(..)
  , dataCiteXML
  ) where

import Control.Arrow (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.URI (URI(..))
import qualified Text.XML.Light as XML

import Ops
import Model.Party.Types
import Model.Funding.Types
import Model.Funding.FundRef

data DataCite = DataCite
  { dataCiteDOI :: Maybe BS.ByteString
  , dataCiteTitle :: T.Text
  , dataCiteAuthors :: [Party]
  , dataCiteYear :: Int
  , dataCiteDescription :: Maybe T.Text
  , dataCiteFunders :: [Funding]
  , dataCitePublication :: Maybe URI
  , dataCiteReferences :: [URI]
  , dataCiteSubjects :: [BS.ByteString]
  }

dataCiteXML :: DataCite -> XML.Element
dataCiteXML DataCite{..} =
  "resource" <=>
    [ "xmlns" =. "http://datacite.org/schema/kernel-4"
    , XML.Attr (q "xsi"){ XML.qPrefix = Just "xmlns" } "http://www.w3.org/2001/XMLSchema-instance"
    , XML.Attr (q "schemaLocation"){ XML.qPrefix = Just "xsi" } "http://datacite.org/schema/kernel-4 http://schema.datacite.org/meta/kernel-4.4/metadata.xsd"
    ] $ catMaybes
    [ Just $ "identifier" <=>
      ("identifierType" =. "DOI")
      $ maybe "(:tba)" BSC.unpack dataCiteDOI
    , Just $ "publisher" <.> "Databrary"
    , Just $ "resourceType" <=>
      ("resourceTypeGeneral" =. "Dataset")
      $ "Volume"
    , Just $ "rightsList" <.> ("rights" <=>
      ("rightsURI" =. "http://databrary.org/access/policies/agreement.html")
      ) "Databrary Access Agreement"
    , Just $ "titles" <.> "title" <.> T.unpack dataCiteTitle
    , "creators" <?> dataCiteAuthors $ \Party{ partyRow = PartyRow{..} } -> "creator" <.> catMaybes
      [ Just $ "creatorName" <.> T.unpack (partySortName <> foldMap (T.pack ", " <>) partyPreName)
      , ("nameIdentifier" <=>
        [ "schemeURI" =. "http://orcid.org/"
        , "nameIdentifierScheme" =. "ORCID"
        ]) . show <$> partyORCID
      , ("affiliation" <.>) . T.unpack <$> partyAffiliation
      ]
    , Just $ "publicationYear" <.> show dataCiteYear
    , ("descriptions" <.>) . ("description" <=>
      ("descriptionType" =. "Abstract"))
      . T.unpack <$> dataCiteDescription
    , "contributors" <?> dataCiteFunders $ \Funding{ fundingFunder = Funder{..} } -> "contributor" <=>
      [ "contributorType" =. "Funder" ] $
      [ "contributorName" <.> T.unpack funderName
      , "nameIdentifier" <=>
        [ "schemeURI" =. "http://crossref.org/fundref"
        , "nameIdentifierScheme" =. "FundRef"
        ] $ fundRefDOI ++ show funderId
      ]
    , "subjects" <?> dataCiteSubjects $ ("subject" <.>) . BSC.unpack
    , (isNothing dataCitePublication || null dataCiteReferences) `unlessUse`
      ("relatedIdentifiers" <.>
        maybe id ((:) . ur "IsSupplementTo") dataCitePublication (map (ur "References") dataCiteReferences))
    ]
  where
  infixr 5 <.>, <=>
  (<.>) :: XML.Node a => String -> a -> XML.Element
  (<.>) = XML.unode
  (<=>) :: XML.Node (a, b) => String -> a -> b -> XML.Element
  (<=>) = curry . XML.unode
  (<?>) :: XML.Node [b] => String -> [a] -> (a -> b) -> Maybe XML.Element
  (<?>) _ [] _ = Nothing
  (<?>) n l f = Just $ n <.> map f l
  (=.) :: String -> String -> XML.Attr
  (=.) = XML.Attr . q
  q = XML.unqual
  ur t u = "relatedIdentifier" <.>
    first (("relationType" =. t :) . return . ("relatedIdentifierType" =.))
      (case uriScheme u of
        "doi:" -> ("DOI", uriPath u)
        "hdl:" -> ("Handle", uriPath u)
        _ -> ("URL", show u))

