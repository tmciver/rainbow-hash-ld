{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.HSPARQL
  ( getRecentFiles
  , getFileForContent
  ) where

import           Protolude

import           Control.Monad.Logger            (LogLevel (LevelDebug, LevelError, LevelInfo))
import           Data.RDF                        (LValue (..), Node (..))
import           Data.Text                       (pack, unpack)
import qualified Data.Text.Encoding              as T
import           Data.Time                       (UTCTime)
import           Data.Time.Format.ISO8601        (iso8601ParseM)
import           Database.HSparql.Connection     (BindingValue (..),
                                                  selectQuery)
import           Database.HSparql.QueryGenerator
import           Network.HTTP.Media              (MediaType, parseAccept)
import           Numeric.Natural                 (Natural)
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
        toFile [fileUriBV, fileNameBV, titleBV, descBV, mediaTypeBV, createdBV, updatedBV, contentUrlBV] = do
          fileUri <- getUri fileUriBV
          maybeFileName <- getPlainLiteralMaybe fileNameBV
          maybeTitle <- getPlainLiteralMaybe titleBV
          maybeDesc <- getPlainLiteralMaybe descBV
          mediaType <- getMediaType mediaTypeBV
          createdAt <- getCreatedAt createdBV
          maybeUpdatedAt <- getUpdatedAt updatedBV
          let updatedAt = fromMaybe createdAt maybeUpdatedAt
          contentUrl <- getUri contentUrlBV
          pure $ File fileUri maybeFileName maybeTitle maybeDesc mediaType createdAt updatedAt contentUrl
        toFile l = throwError $ BindingValueError $ BindingValueCountError (fromIntegral $ length l) 6

        getUri
          :: MonadError HsparqlError m
          => BindingValue
          -> m URI
        getUri = parseUnboundAsError parseUri
          where parseUri
                  :: MonadError HsparqlError m
                  => Node
                  -> m URI
                parseUri (UNode uriText) = case mkURI uriText of
                  Nothing -> throwError $ BindingValueError $ URIParseError uriText
                  Just uri -> pure uri
                parseUri node = throwError $ BindingValueError $ NonURINodeError node

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

        getMediaType
          :: MonadError HsparqlError m
          => BindingValue
          -> m MediaType
        getMediaType = (parseMediaType' =<<) . getPlainLiteral
          where parseMediaType'
                  :: MonadError HsparqlError m
                  => Text
                  -> m MediaType
                parseMediaType' t =
                  case parseAccept $ T.encodeUtf8 t of
                    Just mt -> pure mt
                    Nothing -> throwError $ BindingValueError $ MediaTypeParseError t

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

        parseDateTimeNode
          :: MonadError HsparqlError m
          => Node
          -> m UTCTime
        parseDateTimeNode (LNode (TypedL iso8601Text _)) =
          case iso8601ParseM (unpack iso8601Text) of
            Nothing -> throwError $ DateTimeParseError iso8601Text
            Just t  -> pure t
        parseDateTimeNode node = throwError $ BindingValueError $ NonLiteralNode node

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

recentFilesQuery :: Query SelectQuery
recentFilesQuery = do
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
  nfo <- prefix "nfo" (iriRef "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#")
  schema <- prefix "schema" (iriRef "https://schema.org/")

  fileIri <- var
  name <- var
  label <- var
  desc <- var
  mediaType <- var
  created <- var
  updated <- var
  contentUrl <- var

  triple_ fileIri (rdf .:. "type") (nfo .:. "FileDataObject")
  optional_ (triple_ fileIri (nfo .:. "fileName") name)
  optional_ (triple_ fileIri (rdfs .:. "label") label)
  optional_ (triple_ fileIri (rdfs .:. "comment") desc)
  triple_ fileIri (schema .:. "encodingFormat") mediaType
  triple_ fileIri (nfo .:. "fileCreated") created
  triple_ fileIri (nfo .:. "fileLastModified") updated
  triple_ fileIri (nfo .:. "fileUri") contentUrl

  orderNextDesc created

  limit_ 10

  selectVars [fileIri, name, label, desc, mediaType, created, updated, contentUrl]

getFileForContent :: URI -> URI -> IO (Maybe URI)
getFileForContent contentUrl sparqlEndpoint = do
  -- Log the SPARQL query
  writeLog LevelDebug (pack . createSelectQuery $ recentFilesQuery)

  maybeBvss <- selectQuery (unpack $ render sparqlEndpoint) (fileForContentQuery contentUrl)
  pure $ maybeBvss >>= toUri
  where toUri :: [[BindingValue]] -> Maybe URI
        toUri [[Bound (UNode uriText)]] = mkURI uriText
        toUri _                         = Nothing

fileForContentQuery :: URI -> Query SelectQuery
fileForContentQuery contentUrl = do
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  nfo <- prefix "nfo" (iriRef "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#")

  fileIri <- var
  created <- var

  let contentNode :: Node
      contentNode = UNode $ render contentUrl

  triple_ fileIri (rdf .:. "type") (nfo .:. "FileDataObject")
  triple_ fileIri (nfo .:. "fileUri") contentNode
  triple_ fileIri (nfo .:. "fileCreated") created

  orderNextDesc created

  limit_ 1

  selectVars [fileIri]
