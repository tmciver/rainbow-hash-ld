{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Caldron.App
  ( AppM
  , AppError(..)
  , runApp
  , appErrorToString
  , updateFileContent
  ) where

import           Protolude

import           Text.URI             (URI)
import           Network.HTTP.Media   (MediaType)
import           Control.Monad.Catch           (MonadCatch, MonadMask,
                                                MonadThrow)
import           Control.Monad.Logger          (MonadLogger (..), fromLogStr,
                                                toLogStr, logInfoN)
import           Data.RDF                      (RDF, TList)
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import           System.FilePath               (takeFileName)
import System.IO (hFileSize)

import Caldron.Config            (Config (..))
import Caldron.HSPARQL           as HSPARQL
import Caldron.HTTPClient        as HTTPClient (HTTPClientError,
                                                              httpClientErrorToString,
                                                              mapError,
                                                              postToSPARQL,
                                                              putFile)
import Caldron.LinkedData hiding (updateFileContent)
import qualified Caldron.LinkedData as LD
import Caldron.Logger            (writeLog)
import Caldron.MediaTypeDiscover (discoverMediaTypeFP)
import Caldron.RDF4H             (fileDataToRDF)

data AppError
  = HTTPClientError HTTPClientError
  | SparqlError HSPARQL.SparqlError
  | FileError FileError

appErrorToString :: AppError -> Text
appErrorToString (HTTPClientError hce) = httpClientErrorToString hce
appErrorToString (SparqlError sparqlError) = HSPARQL.sparqlErrorToText sparqlError
appErrorToString (FileError fileError) = fileErrorToText fileError

newtype AppM a = AppM (ExceptT AppError (ReaderT Config IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadError AppError, MonadMask, MonadCatch, MonadThrow)

runApp :: AppM a -> Config -> IO (Either AppError a)
runApp (AppM except) = runReaderT (runExceptT except)

instance FileGet AppM where
  getFile fileUri = asks sparqlEndpoint >>= liftIO . flip HSPARQL.getFile fileUri
  getRecentFiles = asks sparqlEndpoint >>= liftIO . HSPARQL.getRecentFiles
  getFileForContent contentUrl = asks sparqlEndpoint >>= liftIO . HSPARQL.getFileForContent contentUrl

instance FilePut AppM FilePath where
  putFileInStore fp = do
    blobStoreUrl' <- asks blobStoreUrl
    mapError HTTPClientError (HTTPClient.putFile blobStoreUrl' fp)

instance MetadataPut AppM where
  putFileMetadata host blobUrl uploadedBy maybeAuthor maybeFileName size maybeTitle maybeDesc time mt = do
    logInfoN "Converting file metadata to RDF"
    -- generate a graph for the resource
    (url, rdf :: RDF TList) <- liftIO $ fileDataToRDF host blobUrl uploadedBy maybeAuthor maybeFileName size maybeTitle maybeDesc time mt

    -- Debug: print the RDF graph
    -- Why TF do I need to pull out the PrefixMappings and Base URL?
    -- let pm = RDF4H.prefixMappings rdf
    --     base = RDF4H.unBaseUrl <$> RDF4H.baseUrl rdf
    --     serializer = RDF4H.TurtleSerializer base pm
    -- liftIO $ RDF4H.writeRdf serializer rdf

    logInfoN "Preparing to POST data to SPARQL endpoint"

    -- Post the graph to the SPARQL server via the Graph Store Protocol
    gspUri <- asks sparqlEndpoint
    mapError HTTPClientError (postToSPARQL gspUri rdf)

    pure url

  updateFileGraphWithContent host fileUri blobUrl agentUri onBehalfOf maybeFileName size time =
    mapError SparqlError $ HSPARQL.updateFileGraphWithContent host fileUri blobUrl agentUri onBehalfOf maybeFileName size time

instance MediaTypeDiscover AppM FilePath where
  getMediaType = liftIO . discoverMediaTypeFP

instance FileNameGet AppM FilePath where
  getFileName = pure . Just . T.pack . takeFileName

instance FileSizeGet AppM FilePath where
  getFileSize fp = liftIO $ withFile fp ReadMode hFileSize

instance Time AppM where
  getCurrentTime = liftIO Time.getCurrentTime

instance MonadLogger AppM where
  monadLoggerLog _ _ logLevel = liftIO . writeLog logLevel . decodeUtf8 . fromLogStr . toLogStr

updateFileContent
  :: Text -- ^Hostname from HTTP request
  -> URI -- ^URI of File object to be updated
  -> FilePath   -- ^File content
  -> URI -- ^URI of agent putting the file
  -> Maybe URI -- ^URI of the user on whose behalf this file is added (author)
  -> Maybe MediaType
  -> AppM ()
updateFileContent host fileUri filePath uploadedBy maybeOnBehalfOf maybeMT = do
  eitherRes <- LD.updateFileContent host fileUri filePath uploadedBy maybeOnBehalfOf maybeMT
  case eitherRes of
    Left err -> throwError $ FileError err
    Right _ -> pure ()
