{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RainbowHash.RDF4H
  ( fileDataToRDF
  ) where

import           Protolude

import qualified Data.Map                 as Map
import           Data.RDF                 (BaseUrl (..), PrefixMappings (..),
                                           RDF, Rdf, lnode, mkRdf, plainL,
                                           triple, typedL, unode)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import           Data.UUID                (toText)
import           Data.UUID.V4             (nextRandom)
import           Network.HTTP.Media       (MediaType, renderHeader)
import           Text.URI                 (URI, mkURI, render)

-- TODO: Make this generic. It doesn't appear to need IO.
fileDataToRDF
  :: (Rdf a)
  => URI -- ^URI to the bytes of the file content.
  -> URI -- ^URI of the agent that created the file.
  -> Maybe Text -- ^filename
  -> Maybe Text -- ^title
  -> Maybe Text -- ^description
  -> UTCTime
  -> MediaType
  -> IO (URI, RDF a)
fileDataToRDF blobUrl createdByUri maybeFileName maybeTitle maybeDesc time mt = do
  -- FIXME: Replace example.com
  let baseUrlText :: Text
      baseUrlText = "http://example.com/data/"

  fileId <- nextRandom
  fileUri <- mkURI $ baseUrlText <> toText fileId
  fileDataId <- nextRandom
  fileDataUri <- mkURI $ baseUrlText <> toText fileDataId

  let fileUriText = render fileUri
      fileDataUriText = render fileDataUri

      timeISO8601 :: Text
      timeISO8601 = time & iso8601Show & T.pack

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
        -- If there's a title, use that; otherwise use the file name.
        -- FIXME: don't add a label if title and file name are absent
        let label = case (maybeTitle, maybeFileName) of
                      (Just title, _) -> title
                      (_, Just fileName) -> "A file with name \"" <> fileName <> "\""
                      _ -> "A file with no name or title"
        in
          [triple (unode fileUriText) (unode "rdfs:label") (lnode (plainL label))]
        <>
        case maybeDesc of
          Just desc -> [triple (unode fileUriText) (unode "rdfs:comment") (lnode $ plainL desc)]
          Nothing -> []
        <>

        -- Create a FileData object. This is similar to a nfo:FileDataObject but
        -- is meant to represent the immutable state of the file at some point
        -- in time.
        [ triple (unode fileDataUriText) (unode "rdf:type") (unode "fo:FileData")
        , triple (unode fileDataUriText) (unode "fo:fileContent") (unode $ render blobUrl)
        , triple (unode fileDataUriText) (unode "fo:fileCreated") (lnode (typedL timeISO8601 "xsd:dateTime"))
        , triple (unode fileDataUriText) (unode "fo:fileCreatedBy") (unode $ render createdByUri)
        , triple (unode fileDataUriText) (unode "fo:mediaType") (lnode (plainL . T.decodeUtf8 . renderHeader $ mt))
        ]
        <>

        -- Point this file at the above FileData object.
        [ triple (unode fileUriText) (unode "fo:fileData") (unode fileDataUriText) ]
        <>

        -- NEPOMUK File Ontology (nfo)
        [ triple (unode fileUriText) (unode "rdf:type") (unode "nfo:FileDataObject")
        , triple (unode fileUriText) (unode "nfo:fileOwner") (unode $ render createdByUri)
        , triple (unode fileUriText) (unode "nfo:fileUri") (unode $ render blobUrl)
        , triple (unode fileUriText) (unode "nfo:fileCreated") (lnode $ typedL timeISO8601 "xsd:dateTime")
        , triple (unode fileUriText) (unode "nfo:fileLastModified") (lnode $ typedL timeISO8601 "xsd:dateTime")
        ]
        <>

        -- Add some filename triples, if we have it.
        case maybeFileName of
          Just fileName -> [ triple (unode fileUriText) (unode "nfo:fileName") (lnode $ plainL fileName)
                           , triple (unode fileDataUriText) (unode "fo:fileName") (lnode $ plainL fileName)
                           ]
          Nothing -> []
        <>

        -- LDH compatible ontology
        [ triple (unode fileUriText) (unode "rdf:type") (unode "dh:Item") ]
        <>

        -- Schema.org stuff
        [triple (unode fileUriText) (unode "schema:encodingFormat") (lnode (plainL . T.decodeUtf8 . renderHeader $ mt))]
        <>
        case maybeTitle of
          Just title -> [triple (unode fileUriText) (unode "schema:title") (lnode $ plainL title)]
          Nothing -> []

      rdf = mkRdf triples (Just $ BaseUrl baseUrlText) prefixes

  pure (fileUri, rdf)
