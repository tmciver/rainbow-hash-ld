{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.RDF4H
  ( fileDataToRDF
  ) where

import Protolude

import qualified Data.Map as Map
import Data.RDF (Rdf, RDF, PrefixMappings(..), BaseUrl(..), triple, unode, mkRdf)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Text.URI (URI, mkURI, render)

import RainbowHash.MediaType (MediaType, mediaTypeToText)

fileDataToRDF
  :: (Rdf a)
  => URI
  -> Maybe Text -- ^filename
  -> UTCTime
  -> MediaType
  -> IO (URI, RDF a)
fileDataToRDF blobUrl maybeFileName time mt = do
  let baseUrlText :: Text
      baseUrlText = "http://example.com/data/"

  uuid <- nextRandom
  url <- mkURI $ baseUrlText <> toText uuid

  let urlText = render url

      timeISO8601 :: Text
      timeISO8601 = time & iso8601Show & T.pack & show

      prefixes :: PrefixMappings
      prefixes = mempty
        & Map.insert "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        & Map.insert "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
        & Map.insert "nfo" "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
        & Map.insert "xsd" "http://www.w3.org/2001/XMLSchema#"
        & Map.insert "schema" "https://schema.org/"
        & PrefixMappings

      triples =
        -- RDF
        [triple (unode urlText) (unode "rdf:type") (unode "nfo:FileDataObject")]
        <>

        -- RDFS
        let label = case maybeFileName of
                      Just fileName -> "A file with name \"" <> fileName <> "\""
                      Nothing -> "A file with no name"
        in
          [ triple (unode urlText) (unode "rdfs:label") (unode $ show label)
          , triple (unode urlText) (unode "rdfs:comment") (unode $ show label)
          ]
        <>

        -- NEPOMUK File Ontology (nfo)
        [ triple (unode urlText) (unode "nfo:fileUrl") (unode $ render blobUrl)
        , triple (unode urlText) (unode "nfo:fileCreated") (unode $ timeISO8601 <> "^^xsd:dateTime")
        ]
        <>
        case maybeFileName of
          Just fileName -> [triple (unode urlText) (unode "nfo:fileName") (unode $ show fileName)]
          Nothing -> []
        <>

        -- Schema.org stuff
        [triple (unode urlText) (unode "schema:encodingFormat") (unode $ show $ mediaTypeToText mt)]

      rdf = mkRdf triples (Just $ BaseUrl baseUrlText) prefixes

  pure (url, rdf)
