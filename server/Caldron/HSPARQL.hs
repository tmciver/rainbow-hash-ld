{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Caldron.HSPARQL
  ( getRecentFiles
  , getFileForContent
  , getFile
  , getFileAtVersion
  , getRevisionHistory
  , searchFiles
  , searchConcepts
  , getConceptsByUris
  , mintAndCreateConcept
  , updateFileGraphWithContent
  , SparqlError(..)
  , sparqlErrorToText
  ) where

import           Protolude

import Control.Monad.Logger (MonadLogger, logDebugN, LogLevel(LevelDebug, LevelError))
import           Data.UUID                (toText)
import           Data.UUID.V4             (nextRandom)
import qualified Text.Parsec.Error as P
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.ByteString.Lazy                  as LBS
import           Network.HTTP.Client                   (Request, RequestBody(RequestBodyBS), Response, requestHeaders, requestBody, defaultManagerSettings, newManager, httpLbs, responseStatus, responseBody, parseRequest)
import           Network.HTTP.Types                    (Status, statusIsSuccessful)
import           Data.Aeson                      (FromJSON (..), decode,
                                                  withObject, (.:))
import qualified Data.Map.Strict                 as Map
import           Data.RDF                        (LValue (..), Node (..))
import           Data.Text                       (pack, unpack)
import           Data.Time                       (UTCTime)
import           Data.Time.Format.ISO8601        (iso8601ParseM)
import           Database.HSparql.Connection     (BindingValue (..),
                                                  selectQuery)
import           Database.HSparql.QueryGenerator
import           Network.HTTP.Media              (MediaType, parseAccept)
import           Numeric.Natural                 (Natural)
import Text.Mustache (ToMustache(..), object, (~>), (~=), automaticCompile, checkedSubstitute)
import Text.Mustache.Render (SubstitutionError)
import Text.Parsec.Error (ParseError)
import           Text.URI                        (URI, mkURI, render)

import Caldron.Concept             (Concept (..))
import Caldron.File                (File (..), FileRevision (..))
import RainbowHash.Logger              (writeLog)

data HsparqlError
  = BindingValueError BindingValueError
  | DateTimeParseError Text

data BindingValueError
  = BindingValueCountError Natural Natural -- actual, expected
  | UnboundValue
  | URIParseError Text
  | MediaTypeParseError Text
  | LiteralParseError Node
  | NonURINodeError Node
  | NonLiteralNode Node

class ToDisplayText t where
  toDisplayText :: t -> Text

instance ToDisplayText BindingValueError where
  toDisplayText (BindingValueCountError actual expected) =
    "Binding value count error. Got " <> show actual <> " but expected " <> show expected <> "."
  toDisplayText UnboundValue = "Unbound binding."
  toDisplayText (URIParseError t) = "Error trying to parse URI from text: \"" <> t <> "\"."
  toDisplayText (MediaTypeParseError t) = "Error trying to parse media type from text: \"" <> t <> "\"."
  toDisplayText (LiteralParseError node) = "Error trying to parse a literal from node: " <> show node
  toDisplayText (NonURINodeError node) = "Expected a URI node but got " <> show node
  toDisplayText (NonLiteralNode node) = "Expected a literal node but got " <> show node

instance ToDisplayText HsparqlError where
  toDisplayText (BindingValueError bve) = "Binding value error: " <> toDisplayText bve
  toDisplayText (DateTimeParseError t) = "Error when trying to parse a datetime from text: \"" <> t <> "\"."

logSparqlError :: HsparqlError -> IO ()
logSparqlError = writeLog LevelError . toDisplayText

getRecentFiles :: URI -> IO [File]
getRecentFiles sparqlEndpoint = do
  -- Log the SPARQL query
  writeLog LevelDebug (pack . createSelectQuery $ recentFilesQuery)

  maybeBvss <- selectQuery (unpack (render sparqlEndpoint) <> "/query") recentFilesQuery

  -- Log the returned binding values
  writeLog LevelDebug (show maybeBvss)

  let (errors, files) = maybeBvss & maybe [] toFiles
                                  & partitionEithers

  -- log errors
  forM_ errors logSparqlError

  pure files

  where toFiles
          :: MonadError HsparqlError m
          => [[BindingValue]]
          -> [m File]
        toFiles = fmap toFile

        toFile
          :: MonadError HsparqlError m
          => [BindingValue]
          -> m File
        toFile [fileUriBV, fileNameBV, fileSizeBV, titleBV, descBV, mediaTypeBV, createdBV, updatedBV, contentUrlBV, thumbnailBV] = do
          fileUri' <- getUri fileUriBV
          maybeFileName <- getPlainLiteralMaybe fileNameBV
          fileSize' <- getFileSize fileSizeBV
          maybeTitle <- getPlainLiteralMaybe titleBV
          maybeDesc <- getPlainLiteralMaybe descBV
          mediaType <- getMediaType mediaTypeBV
          createdAt <- getCreatedAt createdBV
          maybeUpdatedAt <- getUpdatedAt updatedBV
          let updatedAt = fromMaybe createdAt maybeUpdatedAt
          contentUrl <- getUri contentUrlBV
          maybeThumbnailUri <- getUriMaybe thumbnailBV
          pure $ File fileUri' maybeFileName fileSize' maybeTitle maybeDesc mediaType createdAt updatedAt contentUrl [] maybeThumbnailUri
        toFile l = throwError $ BindingValueError $ BindingValueCountError (fromIntegral $ length l) 10

getFile :: URI -> URI -> IO (Maybe File)
getFile sparqlEndpoint fileUriToGet = do
  let query = fileQuery fileUriToGet
  -- Log the SPARQL query
  writeLog LevelDebug (pack . createSelectQuery $ query)

  maybeBvss <- selectQuery (unpack (render sparqlEndpoint) <> "/query") query

  -- Log the returned binding values
  writeLog LevelDebug (show maybeBvss)

  pure $ case maybeBvss of
    Nothing -> Nothing
    Just [] -> Nothing
    Just rows@(firstRow:_) -> hush $ toFile fileUriToGet rows firstRow

  where toFile
          :: MonadError HsparqlError m
          => URI
          -> [[BindingValue]]
          -> [BindingValue]
          -> m File
        toFile fileUri' rows [fileNameBV, fileSizeBV, titleBV, descBV, mediaTypeBV, createdBV, updatedBV, contentUrlBV, _, thumbnailBV] = do
          maybeFileName <- getPlainLiteralMaybe fileNameBV
          fileSize' <- getFileSize fileSizeBV
          maybeTitle <- getPlainLiteralMaybe titleBV
          maybeDesc <- getPlainLiteralMaybe descBV
          mediaType <- getMediaType mediaTypeBV
          createdAt <- getCreatedAt createdBV
          maybeUpdatedAt <- getUpdatedAt updatedBV
          let updatedAt = fromMaybe createdAt maybeUpdatedAt
          contentUrl <- getUri contentUrlBV
          subjects <- catMaybes <$> mapM subjectFromRow rows
          maybeThumbnailUri <- getUriMaybe thumbnailBV
          pure $ File fileUri' maybeFileName fileSize' maybeTitle maybeDesc mediaType createdAt updatedAt contentUrl subjects maybeThumbnailUri
        toFile _ _ l = throwError $ BindingValueError $ BindingValueCountError (fromIntegral $ length l) 10
        subjectFromRow [_, _, _, _, _, _, _, _, subjectBV, _] = getUriMaybe subjectBV
        subjectFromRow _ = pure Nothing

getFileAtVersion :: URI -> URI -> URI -> IO (Maybe File)
getFileAtVersion sparqlEndpoint fileUri' fileDataUri' = do
  let query = fileAtVersionQuery fileUri' fileDataUri'
  writeLog LevelDebug (pack . createSelectQuery $ query)

  maybeBvss <- selectQuery (unpack (render sparqlEndpoint) <> "/query") query

  writeLog LevelDebug (show maybeBvss)

  pure $ case maybeBvss of
    Nothing          -> Nothing
    Just []          -> Nothing
    Just rows@(firstRow:_) -> hush $ toFile fileUri' rows firstRow

  where toFile
          :: MonadError HsparqlError m
          => URI
          -> [[BindingValue]]
          -> [BindingValue]
          -> m File
        toFile fileUri'' rows [fileNameBV, fileSizeBV, titleBV, descBV, mediaTypeBV, createdBV, versionCreatedBV, contentUrlBV, _, thumbnailBV] = do
          maybeFileName   <- getPlainLiteralMaybe fileNameBV
          fileSize'       <- getFileSize fileSizeBV
          maybeTitle      <- getPlainLiteralMaybe titleBV
          maybeDesc       <- getPlainLiteralMaybe descBV
          mediaType       <- getMediaType mediaTypeBV
          createdAt       <- getCreatedAt createdBV
          versionCreated  <- getCreatedAt versionCreatedBV
          contentUrl      <- getUri contentUrlBV
          subjects        <- catMaybes <$> mapM subjectFromRow rows
          maybeThumbnail  <- getUriMaybe thumbnailBV
          pure $ File fileUri'' maybeFileName fileSize' maybeTitle maybeDesc mediaType createdAt versionCreated contentUrl subjects maybeThumbnail
        toFile _ _ l = throwError $ BindingValueError $ BindingValueCountError (fromIntegral $ length l) 10
        subjectFromRow [_, _, _, _, _, _, _, _, subjectBV, _] = getUriMaybe subjectBV
        subjectFromRow _ = pure Nothing

fileAtVersionQuery :: URI -> URI -> Query SelectQuery
fileAtVersionQuery fileUri' fileDataUri' = do
  fo  <- prefix "fo"  (iriRef "http://timmciver.com/file-ontology#")
  dct <- prefix "dct" (iriRef "http://purl.org/dc/terms/")

  name           <- var
  size           <- var
  title          <- var
  desc           <- var
  mediaType      <- var
  created        <- var
  versionCreated <- var
  contentUrl     <- var
  subject        <- var
  thumbnail      <- var

  let fileIri     = iriRef (render fileUri')
      fileDataIri = iriRef (render fileDataUri')

  optional_ (triple_ fileIri     (fo  .:. "fileName")    name)
  optional_ (triple_ fileIri     (dct .:. "title")       title)
  optional_ (triple_ fileIri     (dct .:. "description") desc)
  triple_            fileIri     (dct .:. "format")      mediaType
  triple_            fileIri     (dct .:. "created")     created
  triple_            fileDataIri (fo  .:. "contentUrl")  contentUrl
  triple_            fileDataIri (fo  .:. "size")        size
  triple_            fileDataIri (dct .:. "created")     versionCreated
  optional_ (triple_ fileIri     (dct .:. "subject")     subject)
  optional_ (triple_ fileDataIri (fo  .:. "thumbnail")   thumbnail)

  selectVars [name, size, title, desc, mediaType, created, versionCreated, contentUrl, subject, thumbnail]

-- | Minimal Aeson types for parsing SPARQL JSON results.
newtype SparqlValue = SparqlValue { svValue :: Text }

instance FromJSON SparqlValue where
  parseJSON = withObject "SparqlValue" $ \o -> SparqlValue <$> o .: "value"

newtype SparqlResults = SparqlResults [Map.Map Text SparqlValue]

instance FromJSON SparqlResults where
  parseJSON = withObject "SparqlResults" $ \o -> do
    results <- o .: "results"
    SparqlResults <$> results .: "bindings"

-- | Fetch all revisions of a file by following the fo:previousRevision chain.
-- Uses a raw SPARQL property path query since hsparql's DSL does not support them.
-- Results are ordered newest-first.
getRevisionHistory :: URI -> URI -> IO [FileRevision]
getRevisionHistory sparqlEndpoint' fileUri' = do
  let queryText = revisionHistoryQuery fileUri'
      reqText   = "POST " <> T.unpack (render sparqlEndpoint') <> "/query"
  case parseRequest reqText of
    Nothing  -> pure []
    Just req -> do
      let req' = req
            { requestHeaders = [ ("Content-Type", "application/sparql-query")
                               , ("Accept",       "application/sparql-results+json")
                               ]
            , requestBody    = RequestBodyBS $ T.encodeUtf8 queryText
            }
      mgr  <- newManager defaultManagerSettings
      resp <- httpLbs req' mgr
      case decode (responseBody resp) of
        Nothing                      -> pure []
        Just (SparqlResults bindings) -> pure $ mapMaybe toRevision bindings
  where
    toRevision binding = do
      uriText        <- svValue <$> Map.lookup "fileDataUri" binding
      createdText    <- svValue <$> Map.lookup "created"     binding
      sizeText       <- svValue <$> Map.lookup "size"        binding
      contentUrlText <- svValue <$> Map.lookup "contentUrl"  binding
      uri            <- mkURI uriText
      created        <- iso8601ParseM (T.unpack createdText)
      size           <- readMaybe sizeText
      contentUrl     <- mkURI contentUrlText
      pure $ FileRevision uri created size contentUrl

revisionHistoryQuery :: URI -> Text
revisionHistoryQuery fileUri' =
  "PREFIX fo: <http://timmciver.com/file-ontology#>\n\
  \PREFIX dct: <http://purl.org/dc/terms/>\n\
  \SELECT ?fileDataUri ?created ?size ?contentUrl\n\
  \WHERE {\n\
  \  {\n\
  \    <" <> render fileUri' <> "> fo:fileData ?fileDataUri .\n\
  \  }\n\
  \  UNION\n\
  \  {\n\
  \    <" <> render fileUri' <> "> fo:fileData/fo:previousRevision+ ?fileDataUri .\n\
  \  }\n\
  \  ?fileDataUri dct:created ?created ;\n\
  \               fo:size ?size ;\n\
  \               fo:contentUrl ?contentUrl .\n\
  \}\n\
  \ORDER BY DESC(?created)"

recentFilesQuery :: Query SelectQuery
recentFilesQuery = do
  -- prefixes
  fo <- prefix "fo" (iriRef "http://timmciver.com/file-ontology#")
  dct <- prefix "dct" (iriRef "http://purl.org/dc/terms/")

  -- variables
  fileIri <- var
  fileDataIri <- var
  name <- var
  size <- var
  title <- var
  desc <- var
  mediaType <- var
  created <- var
  updated <- var
  contentUrl <- var
  thumbnail <- var

  -- where clause
  triple_ fileIri (fo .:. "fileData") fileDataIri
  triple_ fileDataIri (fo .:. "contentUrl") contentUrl
  optional_ (triple_ fileIri (fo .:. "fileName") name)
  optional_ (triple_ fileIri (dct .:. "title") title)
  optional_ (triple_ fileIri (dct .:. "description") desc)
  triple_ fileIri (fo .:. "size") size
  triple_ fileIri (dct .:. "format") mediaType
  triple_ fileIri (dct .:. "created") created
  triple_ fileIri (dct .:. "modified") updated
  optional_ (triple_ fileDataIri (fo .:. "thumbnail") thumbnail)

  orderNextDesc created

  limit_ 10

  selectVars [fileIri, name, size, title, desc, mediaType, created, updated, contentUrl, thumbnail]

fileQuery :: URI -> Query SelectQuery
fileQuery fileUri' = do
  -- prefixes
  fo <- prefix "fo" (iriRef "http://timmciver.com/file-ontology#")
  dct <- prefix "dct" (iriRef "http://purl.org/dc/terms/")

  -- variables
  fileDataIri <- var
  name <- var
  size <- var
  title <- var
  desc <- var
  mediaType <- var
  created <- var
  updated <- var
  contentUrl <- var
  subject <- var
  thumbnail <- var

  -- where clause
  let fileIri = iriRef (render fileUri')
  triple_ fileIri (fo .:. "fileData") fileDataIri
  triple_ fileDataIri (fo .:. "contentUrl") contentUrl
  optional_ (triple_ fileIri (fo .:. "fileName") name)
  optional_ (triple_ fileIri (dct .:. "title") title)
  optional_ (triple_ fileIri (dct .:. "description") desc)
  triple_ fileIri (fo .:. "size") size
  triple_ fileIri (dct .:. "format") mediaType
  triple_ fileIri (dct .:. "created") created
  triple_ fileIri (dct .:. "modified") updated
  optional_ (triple_ fileIri (dct .:. "subject") subject)
  optional_ (triple_ fileDataIri (fo .:. "thumbnail") thumbnail)

  selectVars [name, size, title, desc, mediaType, created, updated, contentUrl, subject, thumbnail]

searchFiles :: URI -> Text -> IO [File]
searchFiles sparqlEndpoint' q = do
  let queryText = searchFilesQuery q
      reqText   = "POST " <> T.unpack (render sparqlEndpoint') <> "/query"
  writeLog LevelDebug $ "searchFiles: SPARQL query:\n" <> queryText
  case parseRequest reqText of
    Nothing  -> do
      writeLog LevelError $ "searchFiles: could not parse request URL: " <> T.pack reqText
      pure []
    Just req -> do
      let req' = req
            { requestHeaders = [ ("Content-Type", "application/sparql-query")
                               , ("Accept",       "application/sparql-results+json")
                               ]
            , requestBody    = RequestBodyBS $ T.encodeUtf8 queryText
            }
      mgr  <- newManager defaultManagerSettings
      resp <- httpLbs req' mgr
      let body = responseBody resp
      writeLog LevelDebug $ "searchFiles: response status: " <> show (responseStatus resp)
      writeLog LevelDebug $ "searchFiles: response body: "   <> T.decodeUtf8 (LBS.toStrict body)
      case decode body of
        Nothing -> do
          writeLog LevelError "searchFiles: failed to decode JSON response"
          pure []
        Just (SparqlResults bindings) -> do
          writeLog LevelDebug $ "searchFiles: got " <> show (length bindings) <> " bindings"
          let (failures, files) = partitionEithers $ map toFile bindings
          forM_ failures $ \reason ->
            writeLog LevelError $ "searchFiles: failed to parse binding: " <> reason
          pure files
  where
    toFile binding = do
      fileUriText    <- note "missing fileUri"    $ svValue <$> Map.lookup "fileUri"    binding
      fileUri'       <- note "invalid fileUri"    $ mkURI fileUriText
      contentUrlText <- note "missing contentUrl" $ svValue <$> Map.lookup "contentUrl" binding
      contentUrl     <- note "invalid contentUrl" $ mkURI contentUrlText
      sizeText       <- note "missing size"       $ svValue <$> Map.lookup "size"       binding
      size           <- note "invalid size"       $ readMaybe sizeText
      mediaTypeText  <- note "missing mediaType"  $ svValue <$> Map.lookup "mediaType"  binding
      mediaType      <- note "invalid mediaType"  $ parseAccept (T.encodeUtf8 mediaTypeText)
      createdText    <- note "missing created"    $ svValue <$> Map.lookup "created"    binding
      created        <- note "invalid created"    $ iso8601ParseM (T.unpack createdText)
      let maybeName      = svValue <$> Map.lookup "name"      binding
          maybeTitle     = svValue <$> Map.lookup "title"     binding
          maybeDesc      = svValue <$> Map.lookup "desc"      binding
          maybeUpdated   = svValue <$> Map.lookup "updated"   binding
                           >>= iso8601ParseM . T.unpack
          updatedAt      = fromMaybe created maybeUpdated
          maybeThumbnail = svValue <$> Map.lookup "thumbnail" binding >>= mkURI
      pure $ File fileUri' maybeName size maybeTitle maybeDesc mediaType created updatedAt contentUrl [] maybeThumbnail

searchFilesQuery :: Text -> Text
searchFilesQuery q =
  "PREFIX text: <http://jena.apache.org/text#>\n\
  \PREFIX fo:   <http://timmciver.com/file-ontology#>\n\
  \PREFIX dct:  <http://purl.org/dc/terms/>\n\
  \SELECT DISTINCT ?fileUri ?name ?size ?title ?desc ?mediaType ?created ?updated ?contentUrl ?thumbnail\n\
  \WHERE {\n\
  \  {\n\
  \    ?fileUri text:query \"" <> escapeSparqlLiteral q <> "\" .\n\
  \    ?fileUri fo:fileData ?fileDataIri .\n\
  \  } UNION {\n\
  \    ?concept text:query \"" <> escapeSparqlLiteral q <> "\" .\n\
  \    ?fileUri dct:subject ?concept .\n\
  \    ?fileUri fo:fileData ?fileDataIri .\n\
  \  }\n\
  \  ?fileDataIri fo:contentUrl ?contentUrl .\n\
  \  OPTIONAL { ?fileUri fo:fileName     ?name  . }\n\
  \  OPTIONAL { ?fileUri dct:title       ?title . }\n\
  \  OPTIONAL { ?fileUri dct:description ?desc  . }\n\
  \  ?fileUri fo:size      ?size      .\n\
  \  ?fileUri dct:format   ?mediaType .\n\
  \  ?fileUri dct:created  ?created   .\n\
  \  OPTIONAL { ?fileUri      dct:modified ?updated   . }\n\
  \  OPTIONAL { ?fileDataIri  fo:thumbnail ?thumbnail . }\n\
  \}\n\
  \ORDER BY DESC(?created)\n\
  \LIMIT 20"

-- TODO: we probably don't want to fetch all Concepts. Update to fetch concepts based on a search term.
allConceptsQuery :: Query SelectQuery
allConceptsQuery = do
  skos <- prefix "skos" (iriRef "http://www.w3.org/2004/02/skos/core#")
  rdf  <- prefix "rdf"  (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

  uri'      <- var
  prefLabel <- var

  triple_ uri' (rdf .:. "type") (skos .:. "Concept")
  triple_ uri' (skos .:. "prefLabel") prefLabel

  selectVars [uri', prefLabel]

toConcept :: [BindingValue] -> Maybe Concept
toConcept [Bound (UNode uriText), Bound (LNode (PlainL label))] =
  fmap (\uri -> Concept uri label) (mkURI uriText)
toConcept _ = Nothing

searchConcepts :: URI -> Text -> IO [Concept]
searchConcepts sparqlEndpoint' q = do
  maybeBvss <- selectQuery (unpack (render sparqlEndpoint') <> "/query") allConceptsQuery
  let concepts = maybe [] (mapMaybe toConcept) maybeBvss
      q' = T.toLower q
  pure $ filter (T.isInfixOf q' . T.toLower . conceptPrefLabel) concepts

getConceptsByUris :: URI -> [URI] -> IO [Concept]
getConceptsByUris _ [] = pure []
getConceptsByUris sparqlEndpoint' uris = do
  maybeBvss <- selectQuery (unpack (render sparqlEndpoint') <> "/query") allConceptsQuery
  let concepts = maybe [] (mapMaybe toConcept) maybeBvss
      uriTexts = map render uris
  pure $ filter ((`elem` uriTexts) . render . conceptUri) concepts

-- | Mint a fresh UUID-based URI for a new SKOS Concept, insert it into the
-- triple store, and return the minted URI.
mintAndCreateConcept :: URI -> Text -> Text -> IO (Either SparqlError URI)
mintAndCreateConcept sparqlEndpoint' host' label = runExceptT $ do
  conceptId   <- liftIO nextRandom
  conceptUri' <- maybe (throwError $ MalformedURI "Could not construct concept URI") pure $
    mkURI $ "https://" <> host' <> "/concepts/" <> toText conceptId
  let endpointUrl = unpack (render sparqlEndpoint') <> "/update"
      sparql      = insertConceptSparql conceptUri' label
  req <- maybe (throwError $ SparqlRequestError "Could not construct SPARQL update request") pure $
    parseRequest ("POST " <> endpointUrl)
  let req' = req { requestHeaders = [("Content-Type", "application/sparql-update")]
                 , requestBody    = RequestBodyBS $ T.encodeUtf8 sparql
                 }
  mgr  <- liftIO $ newManager defaultManagerSettings
  resp <- liftIO $ httpLbs req' mgr
  let status = responseStatus resp
  unless (statusIsSuccessful status) $
    throwError $ SparqlResponseError status
      (T.take 100 . T.decodeUtf8 . LBS.toStrict $ responseBody resp)
  pure conceptUri'

insertConceptSparql :: URI -> Text -> Text
insertConceptSparql conceptUri' label =
  "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\n\
  \PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n\
  \INSERT DATA {\n\
  \  <" <> render conceptUri' <> "> rdf:type skos:Concept ;\n\
  \    skos:prefLabel \"" <> escapeSparqlLiteral label <> "\" .\n\
  \}"

escapeSparqlLiteral :: Text -> Text
escapeSparqlLiteral =
    T.replace "\\" "\\\\"
  . T.replace "\"" "\\\""
  . T.replace "\n" "\\n"
  . T.replace "\r" "\\r"

getFileForContent :: URI -> URI -> IO (Maybe URI)
getFileForContent contentUrl sparqlEndpoint = do
  -- Log the SPARQL query
  writeLog LevelDebug (pack . createSelectQuery $ recentFilesQuery)

  maybeBvss <- selectQuery (unpack $ render sparqlEndpoint) (fileForContentQuery contentUrl)
  pure $ maybeBvss >>= toUri
  where toUri :: [[BindingValue]] -> Maybe URI
        toUri ([Bound (UNode uriText)]:_) = mkURI uriText
        toUri _                         = Nothing

fileForContentQuery :: URI -> Query SelectQuery
fileForContentQuery contentUrl = do
  fo <- prefix "fo" (iriRef "http://timmciver.com/file-ontology#")
  dct <- prefix "dct" (iriRef "http://purl.org/dc/terms/")

  fileIri <- var
  fileDataIri <- var
  created <- var

  let contentNode :: Node
      contentNode = UNode $ render contentUrl

  triple_ fileIri (fo .:. "fileData") fileDataIri
  triple_ fileDataIri (fo .:. "contentUrl") contentNode
  triple_ fileIri (dct .:. "created") created

  orderNextDesc created

  limit_ 1

  selectVars [fileIri]

-- Helper functions for parsing SPARQL responses --

parseBoundNode
  :: MonadError HsparqlError m
  => (Node -> m a)
  -> BindingValue
  -> Maybe (m a)
parseBoundNode _ Unbound      = Nothing
parseBoundNode f (Bound node) = Just (f node)

parseUnboundAsError
  :: MonadError HsparqlError m
  => (Node -> m a)
  -> BindingValue
  -> m a
parseUnboundAsError f bv = case parseBoundNode f bv of
  Nothing -> throwError $ BindingValueError UnboundValue
  Just mv -> mv

parseUri
  :: MonadError HsparqlError m
  => Node
  -> m URI
parseUri (UNode uriText) = case mkURI uriText of
  Nothing -> throwError $ BindingValueError $ URIParseError uriText
  Just uri -> pure uri
parseUri node = throwError $ BindingValueError $ NonURINodeError node

getUri
  :: MonadError HsparqlError m
  => BindingValue
  -> m URI
getUri = parseUnboundAsError parseUri

getUriMaybe
  :: MonadError HsparqlError m
  => BindingValue
  -> m (Maybe URI)
getUriMaybe = sequence . parseBoundNode parseUri

parsePlainLiteralNode
  :: MonadError HsparqlError m
  => Node
  -> m Text
parsePlainLiteralNode (LNode (PlainL t)) = pure t
parsePlainLiteralNode n = throwError $ BindingValueError $ LiteralParseError n

getPlainLiteralMaybe
  :: MonadError HsparqlError m
  => BindingValue
  -> m (Maybe Text)
getPlainLiteralMaybe = sequence . parseBoundNode parsePlainLiteralNode

getPlainLiteral
  :: MonadError HsparqlError m
  => BindingValue
  -> m Text
getPlainLiteral = parseUnboundAsError parsePlainLiteralNode

parseMediaType'
  :: MonadError HsparqlError m
  => Text
  -> m MediaType
parseMediaType' t =
  case parseAccept $ T.encodeUtf8 t of
    Just mt -> pure mt
    Nothing -> throwError $ BindingValueError $ MediaTypeParseError t

getMediaType
  :: MonadError HsparqlError m
  => BindingValue
  -> m MediaType
getMediaType = (parseMediaType' =<<) . getPlainLiteral

parseDateTimeNode
  :: MonadError HsparqlError m
  => Node
  -> m UTCTime
parseDateTimeNode (LNode (TypedL iso8601Text _)) =
  case iso8601ParseM (unpack iso8601Text) of
    Nothing -> throwError $ DateTimeParseError iso8601Text
    Just t  -> pure t
parseDateTimeNode node = throwError $ BindingValueError $ NonLiteralNode node

getCreatedAt
  :: MonadError HsparqlError m
  => BindingValue
  -> m UTCTime
getCreatedAt = parseUnboundAsError parseDateTimeNode

getUpdatedAt
  :: MonadError HsparqlError m
  => BindingValue
  -> m (Maybe UTCTime)
getUpdatedAt = sequence . parseBoundNode parseDateTimeNode

parseFileSizeNode
  :: MonadError HsparqlError m
  => Node
  -> m Integer
parseFileSizeNode node@(LNode (TypedL sizeText _)) =
  case readMaybe sizeText of
    Nothing -> throwError $ BindingValueError $ LiteralParseError node
    Just s  -> pure s
parseFileSizeNode node = throwError $ BindingValueError $ NonLiteralNode node

getFileSize
  :: MonadError HsparqlError m
  => BindingValue
  -> m Integer
getFileSize = parseUnboundAsError parseFileSizeNode

mkURI'
  :: MonadError SparqlError m
  => Text
  -> m URI
mkURI' t = maybe (throwError $ MalformedURI t) pure (mkURI t)

updateFileGraphWithContent
    :: ( MonadIO m
       , MonadReader env m
       , HasField "sparqlEndpoint" env URI
       , MonadError SparqlError m
       , MonadLogger m
       )
    => Text -- ^host name
    -> URI -- ^File object URI
    -> URI -- ^URI of file data in blob storage
    -> URI -- ^URI of agent creating the file
    -> Maybe URI -- ^URI of the user on whose behalf this file is added (author)
    -> Maybe Text -- ^file name
    -> Integer    -- ^file size
    -> UTCTime -- ^file creation time
    -> Maybe URI -- ^URI of thumbnail in blob storage
    -> m ()
updateFileGraphWithContent host fileUri blobUrl agentUri onBehalfOf mFileName size time maybeThumbnailUri = do
  let baseUrlText = "https://" <> host
      fileTitle = fromMaybe "" mFileName
  fileDataId <- liftIO nextRandom
  fileDataUri <- mkURI' $ baseUrlText <> "/file-data/" <> toText fileDataId

  let pfd = PutFileData fileUri fileDataUri blobUrl fileTitle agentUri onBehalfOf size time maybeThumbnailUri
  renderPutFileTemplate pfd >>= logSparql >>= sparqlUpdate
  where logSparql :: MonadLogger m => Text -> m Text
        logSparql t = logDebugN t >> pure t

data PutFileData = PutFileData
  { fileUri :: URI
  , fileDataUri :: URI
  , fileContentUrl :: URI
  , fileTitle :: Text
  , creatorUri :: URI
  , onBehalfOf :: Maybe URI
  , fileSize :: Integer
  , creationTime :: UTCTime
  , thumbnailUri :: Maybe URI
  }

instance ToMustache PutFileData where
  toMustache PutFileData{..} =
    let activityTitle :: Text
        activityTitle = "Modify activity" <>
          if T.null fileTitle
          then ""
          else " for '" <> fileTitle <> "'"
        delegationTitle :: Text
        delegationTitle = "Delegation for upload" <>
          if T.null fileTitle
          then ""
          else " for '" <> fileTitle <> "'"
    in object
    [ "fileUri" ~> render fileUri
    , "fileDataUri" ~> render fileDataUri
    , "fileContentUrl" ~> render fileContentUrl
    , "fileTitle" ~> fileTitle
    , "creatorUri" ~> render creatorUri
    , "onBehalfOf" ~> (render <$> onBehalfOf)
    , "hasDelegate" ~> isJust onBehalfOf
    , "delegationTitle" ~> delegationTitle
    , "activityTitle" ~> activityTitle
    , "size" ~> fileSize
    , "creationTime" ~= creationTime
    , "thumbnailUri" ~> (render <$> thumbnailUri)
    , "hasThumbnail" ~> isJust thumbnailUri
    ]

data SparqlError
  = TemplateCompileError ParseError
  | TemplateSubstitueError [SubstitutionError]
  | SparqlResponseError Status Text
  | SparqlRequestError Text
  | MalformedURI Text

sparqlErrorToText :: SparqlError -> Text
sparqlErrorToText (TemplateCompileError pe) =
  let errMsg = pe & P.errorMessages
                 <&> T.pack . P.messageString
                  & T.intercalate ", "
  in "SPARQL template compile error: " <> errMsg
sparqlErrorToText (TemplateSubstitueError ses) =
  let errMsg = ses <&> show
                    & T.intercalate ", "
  in "SPARQL template substitution error: " <> errMsg
sparqlErrorToText (SparqlResponseError status  msg) =
  "SPARQL response error: status: " <> show status <> ", error message: " <> msg
sparqlErrorToText (SparqlRequestError msg) =
  "SPARQL request error: " <> msg
sparqlErrorToText (MalformedURI t) = "Malformed URI: " <> t

renderPutFileTemplate
  :: ( MonadIO m
     , MonadError SparqlError m
     )
  => PutFileData
  -> m Text
renderPutFileTemplate putFileData = do
  let searchSpace = ["./template"]
      templateName = "put-file.template"

  compiled <- liftIO $ automaticCompile searchSpace templateName
  case compiled of
    Left err -> throwError $ TemplateCompileError err --writeLog LevelError $ "Error rendering Mustache template: " <> show err
    Right template ->
      case checkedSubstitute template putFileData of
        ([], t) -> putStrLn t >> pure t
        (errs, _) -> throwError $ TemplateSubstitueError errs

sparqlUpdate
  :: ( MonadIO m
     , MonadReader env m
     , HasField "sparqlEndpoint" env URI
     , MonadError SparqlError m
     )
  => -- URI -- ^SPARQL endpoint
  Text -- ^SPARQL update payload as text
  -> m ()
sparqlUpdate payload = do
  mgr <- liftIO $ newManager defaultManagerSettings
  req <- mkRequest payload
  (liftIO $ httpLbs req mgr) >>= responseToError

checkStatusWithTextMessage
  :: MonadError SparqlError m
  => (a -> Text)
  -> Response a
  -> m ()
checkStatusWithTextMessage toText' resp = do
  let status = responseStatus resp
  unless (statusIsSuccessful status) $
    let msg = T.take 100 . toText' . responseBody $ resp
    in throwError $ SparqlResponseError status msg

responseToError
  :: ( MonadError SparqlError m )
  => Response LBS.ByteString
  -> m ()
responseToError = checkStatusWithTextMessage (T.take 100 . T.decodeUtf8 . LBS.toStrict)

mkRequest
  :: ( MonadError SparqlError m
     , MonadIO m
     , MonadReader env m
     , HasField "sparqlEndpoint" env URI
     )
  => Text
  -> m Request
mkRequest t = do
  config <- ask
  let sparqlEndpoint' = getField @"sparqlEndpoint" config

  -- initialize the Request object
  let reqText = "POST " <> T.unpack (render sparqlEndpoint') <> "/update"

  req <- case parseRequest reqText of
           Just r -> pure r
           Nothing -> throwError $ SparqlRequestError $ "Could not create Request from text \"" <> T.pack reqText <> "\""

  -- update the headers
  let req' = req { requestHeaders = ("Content-Type", "application/sparql-update") : requestHeaders req
                 , requestBody = RequestBodyBS $ T.encodeUtf8 t
                 }

  pure req'
