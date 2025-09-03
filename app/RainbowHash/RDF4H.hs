{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RainbowHash.RDF4H
  ( fileDataToRDF
  ) where

import           Protolude

import Control.Monad.Logger (MonadLogger, logInfoN, logDebugN, logErrorN)
import qualified Data.Map                 as Map
import           Data.RDF                 (BaseUrl (..), PrefixMappings (..),
                                           RDF, Rdf, lnode, mkRdf, plainL,
                                           triple, typedL, unode, TurtleParser)
import qualified           Data.RDF as RDF
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import           Data.UUID                (toText)
import           Data.UUID.V4             (nextRandom)
import           Network.HTTP.Media       (MediaType, renderHeader)
import           Text.URI                 (URI, mkURI, render)

import RainbowHash.User (WebID)
import RainbowHash.Crypto (ProfileData)

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

newtype RDF4HError = RDFParseError RDF.ParseFailure

getProfileGraph
  :: ( Rdf a
     , MonadError RDF4HError m
     , MonadLogger m
     , MonadIO m
     )
  => WebID
  -> m (RDF a)
getProfileGraph webId' = do
  logInfoN $ "Fetching profile document from " <> render webId'
  let url = toS . render $ webId'
      turtleParser :: TurtleParser
      turtleParser = TurtleParser Nothing Nothing
  eitherGraph <- liftIO $ RDF.parseURL turtleParser url
  case eitherGraph of
    Left err    -> throwError $ RDFParseError err
    Right graph -> pure graph

parseProfileData
  :: forall m a.
  ( MonadError RDF4HError m
  , MonadLogger m
  , Rdf a
  )
  => RDF a
  -> m ProfileData
parseProfileData g = do
  logInfoN "Fetching name from user profile document."

  name <- queryName g
  certData <- queryCertData g

  pure ProfileData {..}

queryCertData
  :: ( MonadError RDF4HError m
     , MonadLogger m
     , Rdf a
     )
  => RDF a
  -> m (NonEmpty CertificateData)
queryCertData g = do
  logInfoN "Querying certificate data from user profile document."
  logDebugN $ "Profile graph: " <> T.pack (RDF.showGraph g)
  let triples = RDF.query g Nothing (Just (RDF.UNode "http://www.w3.org/ns/auth/cert#key")) Nothing
      certNodes = object <$> triples
      eitherCertData = getCertData g <$> certNodes
      (errs, certData') = partitionEithers eitherCertData

  logDebugN $ "Number of Certificate nodes found: " <> show (length certNodes)
  logDebugN $ "Certificate nodes found: " <> show certNodes

  -- log errors
  forM_ errs logErrorN

  case nonEmpty certData' of
    Nothing -> throwError $ CertificateError "Could not read certificate data from profile document."
    Just certData'' -> pure certData''

object :: RDF.Triple -> RDF.Subject
object (RDF.Triple _ _ o) = o

getCertData
  :: ( MonadError Text m
     , Rdf a
     )
  => RDF a
  -> RDF.Node
  -> m CertificateData
getCertData g' n = CertificateData <$> getModulus g' n <*> getExponent g' n

getModulus
  :: ( MonadError Text m
     , Rdf a
     )
  => RDF a
  -> RDF.Node
  -> m Integer
getModulus g n = do
  let modTriples = RDF.query g (Just n) (Just (RDF.UNode "http://www.w3.org/ns/auth/cert#modulus")) Nothing
  case modTriples of
    [] -> throwError "Could not retrieve certificate modulus from profile data."
    -- FIXME: consider all literal nodes, not just the typed version.
    [RDF.Triple _ _ (RDF.LNode (RDF.TypedL modHexStr _))] ->
      case T.hexadecimal modHexStr of
        Left s -> throwError $ "Could not read certificate modulus from profile data: " <> T.pack s
        Right (i, _) -> pure i
    triples -> throwError $ "Could not read certificate modulus from profile data: " <> show triples

getExponent
  :: ( MonadError Text m
     , Rdf a
     )
  => RDF a
  -> RDF.Node
  -> m Integer
getExponent g n = do
  let expTriples = RDF.query g (Just n) (Just (RDF.UNode "http://www.w3.org/ns/auth/cert#exponent")) Nothing
  case expTriples of
    [] -> throwError "Could not retrieve certificate exponent from profile data."
    -- FIXME: consider all literal nodes, not just the typed version.
    [RDF.Triple _ _ (RDF.LNode (RDF.TypedL expStr _))] ->
      case readMaybe expStr of
        Nothing -> throwError "Could not read certificate exponent from profile data."
        Just i -> pure i
    triples -> throwError $ "Could not read certificate exponent from profile data: " <> show triples

queryName
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> m (Maybe Text)
queryName g' = runMaybeT $ queryFirstNameFOAF g' <|> queryFirstNameSchema g' <|> queryNameFOAF g'

queryFirstNameFOAF
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> MaybeT m Text
queryFirstNameFOAF g' = do
  let triples = RDF.query g' Nothing (Just (RDF.UNode "http://xmlns.com/foaf/0.1/givenName")) Nothing
  case triples of
    [] -> empty
    -- FIXME: look at all triples, not just the first
    triple:_ -> case triple of
      -- FIXME: consider all literal nodes, not just the plain version.
      RDF.Triple _ _ (RDF.LNode (RDF.PlainL firstName)) -> pure firstName
      _ -> empty

queryFirstNameSchema
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> MaybeT m Text
queryFirstNameSchema g' = do
  let triples = RDF.query g' Nothing (Just (RDF.UNode "https://schema.org/givenName")) Nothing
  case triples of
    [] -> empty
    -- FIXME: look at all triples, not just the first
    triple:_ -> case triple of
      -- FIXME: consider all literal nodes, not just the plain version.
      RDF.Triple _ _ (RDF.LNode (RDF.PlainL firstName)) -> pure firstName
      _ -> empty

queryNameFOAF
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> MaybeT m Text
queryNameFOAF g' = do
  let triples = RDF.query g' Nothing (Just (RDF.UNode "http://xmlns.com/foaf/0.1/name")) Nothing
  case triples of
    [] -> empty
    -- FIXME: look at all triples, not just the first
    triple:_ -> case triple of
      -- FIXME: consider all literal nodes, not just the plain version.
      RDF.Triple _ _ (RDF.LNode (RDF.PlainL firstName)) -> pure firstName
      _ -> empty

getProfileData
  :: ( MonadError RDF4HError m
     , MonadLogger m
     , MonadIO m
     )
  => WebID
  -> m ProfileData
getProfileData webId' =
  getProfileGraph @RDF.TList webId' >>= parseProfileData

