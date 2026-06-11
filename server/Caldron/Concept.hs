{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Concept
  ( Concept (..)
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

lookupConcept :: [Concept] -> URI -> Maybe Concept
lookupConcept concepts uri = find ((== render uri) . render . conceptUri) concepts
