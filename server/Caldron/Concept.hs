{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Concept
  ( Concept (..)
  , lookupConcept
  , searchConcepts
  ) where

import Protolude

import Data.Aeson  (ToJSON (..), object, (.=))
import qualified Data.Text as T
import Text.URI    (URI, mkURI, render)

data Concept = Concept
  { conceptUri       :: URI
  , conceptPrefLabel :: Text
  }

instance ToJSON Concept where
  toJSON c = object
    [ "uri"       .= render (conceptUri c)
    , "prefLabel" .= conceptPrefLabel c
    ]

testConcepts :: [Concept]
testConcepts = mapMaybe makeConcept
  [ ("https://example.com/concepts/haskell",      "Haskell")
  , ("https://example.com/concepts/linked-data",  "Linked Data")
  , ("https://example.com/concepts/sparql",       "SPARQL")
  , ("https://example.com/concepts/rdf",          "RDF")
  , ("https://example.com/concepts/skos",         "SKOS")
  , ("https://example.com/concepts/web",          "Web")
  , ("https://example.com/concepts/programming",  "Programming")
  , ("https://example.com/concepts/open-data",    "Open Data")
  ]
  where
    makeConcept (u, l) = do
      uri <- mkURI u
      pure $ Concept uri l

lookupConcept :: URI -> Maybe Concept
lookupConcept uri = find ((== render uri) . render . conceptUri) testConcepts

searchConcepts :: Text -> [Concept]
searchConcepts q =
  let q' = T.toLower q
  in filter (T.isInfixOf q' . T.toLower . conceptPrefLabel) testConcepts
