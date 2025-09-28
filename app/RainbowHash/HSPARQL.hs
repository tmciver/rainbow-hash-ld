{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RainbowHash.HSPARQL
  ( getRecentFiles
  , getFileForContent
  , getFile
  , updateFileGraphWithContent
  , SparqlError(..)
  , sparqlErrorToText
  ) where

import           Protolude

import qualified Text.Parsec.Error as P
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.ByteString.Lazy                  as LBS
import           Network.HTTP.Client                   (Request, RequestBody(RequestBodyBS), Response, requestHeaders, requestBody, defaultManagerSettings, newManager, httpLbs, responseStatus, responseBody, parseRequest)
import           Network.HTTP.Types                    (Status, statusIsSuccessful)
import           Control.Monad.Logger            (LogLevel (LevelDebug, LevelError))
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

import           RainbowHash.File                (File (..))
import           RainbowHash.Logger              (writeLog)

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

  maybeBvss <- selectQuery (unpack $ render sparqlEndpoint) recentFilesQuery

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
        toFile [fileUriBV, fileNameBV, fileSizeBV, titleBV, descBV, mediaTypeBV, createdBV, updatedBV, contentUrlBV] = do
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
          pure $ File fileUri' maybeFileName fileSize' maybeTitle maybeDesc mediaType createdAt updatedAt contentUrl
        toFile l = throwError $ BindingValueError $ BindingValueCountError (fromIntegral $ length l) 9

getFile :: URI -> URI -> IO (Maybe File)
getFile sparqlEndpoint fileUriToGet = do
  let query = fileQuery fileUriToGet
  -- Log the SPARQL query
  writeLog LevelDebug (pack . createSelectQuery $ query)

  maybeBvss <- selectQuery (unpack $ render sparqlEndpoint) query

  -- Log the returned binding values
  writeLog LevelDebug (show maybeBvss)

  pure $ case maybeBvss of
    Nothing -> Nothing
    Just [] -> Nothing
    Just (bvs:_) -> hush $ toFile fileUriToGet bvs

  where toFile
          :: MonadError HsparqlError m
          => URI
          -> [BindingValue]
          -> m File
        toFile fileUri' [fileNameBV, fileSizeBV, titleBV, descBV, mediaTypeBV, createdBV, updatedBV, contentUrlBV] = do
          maybeFileName <- getPlainLiteralMaybe fileNameBV
          fileSize' <- getFileSize fileSizeBV
          maybeTitle <- getPlainLiteralMaybe titleBV
          maybeDesc <- getPlainLiteralMaybe descBV
          mediaType <- getMediaType mediaTypeBV
          createdAt <- getCreatedAt createdBV
          maybeUpdatedAt <- getUpdatedAt updatedBV
          let updatedAt = fromMaybe createdAt maybeUpdatedAt
          contentUrl <- getUri contentUrlBV
          pure $ File fileUri' maybeFileName fileSize' maybeTitle maybeDesc mediaType createdAt updatedAt contentUrl
        toFile _ l = throwError $ BindingValueError $ BindingValueCountError (fromIntegral $ length l) 8

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

  orderNextDesc created

  limit_ 10

  selectVars [fileIri, name, size, title, desc, mediaType, created, updated, contentUrl]

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

  limit_ 1

  selectVars [name, size, title, desc, mediaType, created, updated, contentUrl]

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

updateFileGraphWithContent
    :: ( MonadIO m
       , MonadReader env m
       , HasField "sparqlEndpoint" env URI
       , MonadError SparqlError m
       )
    => URI -- ^File object URI
    -> URI -- ^URI of file data in blob storage
    -> URI -- ^URI of agent creating the file
    -> Integer    -- ^file size
    -> UTCTime -- ^file creation time
    -> m ()
updateFileGraphWithContent fileUri blobUrl agentUri size time = do
  let pfd = PutFileData fileUri blobUrl agentUri size time
  renderPutFileTemplate pfd >>= sparqlUpdate

data PutFileData = PutFileData
  { fileObjectUrl :: URI
  , fileContentUrl :: URI
  , agentUri :: URI
  , fileSize :: Integer
  , creationTime :: UTCTime
  }

instance ToMustache PutFileData where
  toMustache PutFileData{..} = object
    [ "fileObjectUrl" ~> render fileObjectUrl
    , "fileContentUrl" ~> render fileContentUrl
    , "agentUri" ~> render agentUri
    , "size" ~> fileSize
    , "creationTime" ~= creationTime
    ]

data SparqlError
  = TemplateCompileError ParseError
  | TemplateSubstitueError [SubstitutionError]
  | SparqlResponseError Status Text
  | SparqlRequestError Text

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

renderPutFileTemplate
  :: ( MonadIO m
     , MonadError SparqlError m
     )
  => PutFileData
  -> m Text
renderPutFileTemplate putFileData = do
  let searchSpace = ["../../template"]
      templateName = "put-file.mustache"

  compiled <- liftIO $ automaticCompile searchSpace templateName
  case compiled of
    Left err -> throwError $ TemplateCompileError err --writeLog LevelError $ "Error rendering Mustache template: " <> show err
    Right template ->
      case checkedSubstitute template putFileData of
        ([], t) -> pure t
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
checkStatusWithTextMessage toText resp = do
  let status = responseStatus resp
  unless (statusIsSuccessful status) $
    let msg = T.take 100 . toText . responseBody $ resp
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
