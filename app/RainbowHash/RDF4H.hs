{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RainbowHash.RDF4H
  ( fileDataToRDF
  ) where

import           Protolude

import qualified Data.Map                 as Map
import           Data.RDF                 (Triple, Node, BaseUrl (..), PrefixMappings (..),
                                           RDF, Rdf, lnode, mkRdf, plainL,
                                           triple, typedL, unode)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
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
fileDataToRDF blobUrl agentUri maybeFileName maybeTitle maybeDesc time mt = do
  -- FIXME: Replace example.com
  let baseUrlText :: Text
      baseUrlText = "http://example.com/data/"

  fileId <- nextRandom
  fileUri <- mkURI $ baseUrlText <> toText fileId
  fileDataId <- nextRandom
  fileDataUri <- mkURI $ baseUrlText <> toText fileDataId

  let fileUriNode = unode $ render fileUri
      fileDataUriNode = unode $ render fileDataUri

      timeISO8601 :: Text
      timeISO8601 = time & iso8601Show & T.pack

      prefixes :: PrefixMappings
      prefixes = mempty
        & Map.insert "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        & Map.insert "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
        & Map.insert "dct" "http://purl.org/dc/terms/"
        & Map.insert "xsd" "http://www.w3.org/2001/XMLSchema#"
        & Map.insert "dh" "https://www.w3.org/ns/ldt/document-hierarchy#"
        & Map.insert "fo" "http://timmciver.com/file-ontology#"
        & Map.insert "prov" "http://www.w3.org/ns/prov#"
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
          [triple fileUriNode (unode "rdfs:label") (lnode (plainL label))]
        <>
        case maybeDesc of
          Just desc -> [ triple fileUriNode (unode "rdfs:comment") (lnode $ plainL desc)
                       , triple fileUriNode (unode "dct:description") (lnode $ plainL desc)
                       ]
          Nothing -> []
        <>

        -- Create a FileData object.
        [ triple fileDataUriNode (unode "rdf:type") (unode "fo:FileData")
        , triple fileDataUriNode (unode "fo:contentUrl") (unode $ render blobUrl)
        , triple fileDataUriNode (unode "dct:format") (lnode (plainL . TE.decodeUtf8 . renderHeader $ mt))
        , triple fileDataUriNode (unode "dct:created") (lnode (typedL timeISO8601 "xsd:dateTime"))
        , triple fileDataUriNode (unode "dct:creator") (unode $ render agentUri)
        ]
        <>

        -- Point this file at the above FileData object.
        [ triple fileUriNode (unode "rdf:type") (unode "fo:File")
        , triple fileUriNode (unode "fo:fileData") fileDataUriNode
        , triple fileUriNode (unode "dct:created") (lnode (typedL timeISO8601 "xsd:dateTime"))
        , triple fileUriNode (unode "dct:modified") (lnode (typedL timeISO8601 "xsd:dateTime"))
        , triple fileUriNode (unode "dct:format") (lnode (plainL . TE.decodeUtf8 . renderHeader $ mt))
        ]
        <>

        -- Add some filename triples, if we have it.
        case maybeFileName of
          Just fileName -> [ triple fileUriNode (unode "fo:fileName") (lnode $ plainL fileName)
                           , triple fileDataUriNode (unode "fo:fileName") (lnode $ plainL fileName)
                           ]
          Nothing -> []
        <>

        -- LDH compatible ontology
        [ triple fileUriNode (unode "rdf:type") (unode "dh:Item") ]
        <>

        -- Title
        case maybeTitle of
          Just title -> [triple fileUriNode (unode "dct:title") (lnode $ plainL title)]
          Nothing -> []
        <>

        -- PROV
        [ triple fileUriNode (unode "rdf:type") (unode "prov:Entity")
        , triple fileUriNode (unode "prov:wasAttributedTo") (unode $ render agentUri)
        , triple fileUriNode (unode "prov:generatedAtTime") (lnode $ typedL timeISO8601 "xsd:dateTime")
        , triple fileDataUriNode (unode "rdf:type") (unode "prov:Entity")
        , triple fileDataUriNode (unode "prov:wasAttributedTo") (unode $ render agentUri)
        , triple fileDataUriNode (unode "prov:generatedAtTime") (lnode $ typedL timeISO8601 "xsd:dateTime")
        ]

      rdf = mkRdf triples (Just $ BaseUrl baseUrlText) prefixes

  pure (fileUri, rdf)
