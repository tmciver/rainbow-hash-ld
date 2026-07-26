{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Concept
  ( Concept (..)
  , ConceptDetail (..)
  , lookupConcept
  ) where

import Protolude

import Data.Aeson  (ToJSON (..), object, (.=))
import Text.URI    (URI, render)

data Concept = Concept
  { conceptUri       :: URI
  , conceptPrefLabel :: Text
  }

instance ToJSON Concept where
  toJSON c = object
    [ "uri"       .= render (conceptUri c)
    , "prefLabel" .= conceptPrefLabel c
    ]

data ConceptDetail = ConceptDetail
  { cdConcept  :: Concept
  , cdBroader  :: [Concept]
  , cdNarrower :: [Concept]
  }

lookupConcept :: [Concept] -> URI -> Maybe Concept
lookupConcept concepts uri = find ((== render uri) . render . conceptUri) concepts
