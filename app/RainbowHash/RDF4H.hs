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

  fileId <- nextRandom
  fileUri <- mkURI $ baseUrlText <> toText fileId
  fileDataId <- nextRandom
  fileDataUri <- mkURI $ baseUrlText <> toText fileDataId

  let fileUriText = render fileUri
      fileDataUriText = render fileDataUri

      timeISO8601 :: Text
      timeISO8601 = time & iso8601Show & T.pack & show

      prefixes :: PrefixMappings
      prefixes = mempty
        & Map.insert "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        & Map.insert "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
        & Map.insert "nfo" "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
        & Map.insert "xsd" "http://www.w3.org/2001/XMLSchema#"
        & Map.insert "schema" "https://schema.org/"
        & Map.insert "dh" "https://www.w3.org/ns/ldt/document-hierarchy#"
        & Map.insert "fo" "http://timmciver.com/file-ontology#"
        & PrefixMappings

      triples =

        -- RDFS
        let label = case maybeFileName of
                      Just fileName -> "A file with name \"" <> fileName <> "\""
                      Nothing -> "A file with no name"
        in
          [ triple (unode fileUriText) (unode "rdfs:label") (unode $ show label)
          , triple (unode fileUriText) (unode "rdfs:comment") (unode $ show label)
          ]
        <>

        -- Create a FileData object. This is similar to a nfo:FileDataObject but
        -- is meant to represent the immutable state of the file at some point
        -- in time.
        [ triple (unode fileDataUriText) (unode "rdf:type") (unode "nfo:FileData")
        , triple (unode fileDataUriText) (unode "fo:fileContent") (unode $ render blobUrl)
        , triple (unode fileDataUriText) (unode "fo:fileCreated") (unode $ timeISO8601 <> "^^xsd:dateTime")
        , triple (unode fileDataUriText) (unode "fo:mediaType") (unode $ show $ mediaTypeToText mt)
        ]
        <>

        -- Point this file at the above FileData object.
        [ triple (unode fileUriText) (unode "fo:fileData") (unode fileDataUriText)]
        <>

        -- NEPOMUK File Ontology (nfo)
        [ triple (unode fileUriText) (unode "rdf:type") (unode "nfo:FileDataObject")
        , triple (unode fileUriText) (unode "nfo:fileUri") (unode $ render blobUrl)
        , triple (unode fileUriText) (unode "nfo:fileCreated") (unode $ timeISO8601 <> "^^xsd:dateTime")
        , triple (unode fileUriText) (unode "nfo:fileLastModified") (unode $ timeISO8601 <> "^^xsd:dateTime")
        ]
        <>

        -- Add the filename, if we have it.
        case maybeFileName of
          Just fileName -> [ triple (unode fileUriText) (unode "nfo:fileName") (unode $ show fileName)
                           , triple (unode fileDataUriText) (unode "fo:fileName") (unode $ show fileName)
                           ]
          Nothing -> []
        <>

        -- LDH compatible ontology
        [ triple (unode fileUriText) (unode "rdf:type") (unode "dh:Item") ]
        <>

        -- Schema.org stuff
        [triple (unode fileUriText) (unode "schema:encodingFormat") (unode $ show $ mediaTypeToText mt)]

      rdf = mkRdf triples (Just $ BaseUrl baseUrlText) prefixes

  pure (fileUri, rdf)
