{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module RainbowHash.App
  ( AppM
  , AppError(..)
  , runApp
  , appErrorToString
  ) where

import           Protolude

import           Control.Monad.Catch           (MonadCatch, MonadMask,
                                                MonadThrow)
import           Control.Monad.Logger          (MonadLogger (..), fromLogStr,
                                                toLogStr, logInfoN)
import           Data.RDF                      (RDF, TList)
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import           System.FilePath               (takeFileName)
import System.IO (hFileSize)

import           RainbowHash.Config            (Config (..))
import qualified RainbowHash.HSPARQL           as HSPARQL
import           RainbowHash.HTTPClient        as HTTPClient (HTTPClientError,
                                                              httpClientErrorToString,
                                                              mapError,
                                                              postToSPARQL,
                                                              putFile)
import           RainbowHash.LinkedData
import           RainbowHash.Logger            (writeLog)
import           RainbowHash.MediaTypeDiscover (discoverMediaTypeFP)
import           RainbowHash.RDF4H             (fileDataToRDF)

data AppError
  = HTTPClientError HTTPClientError
  | SparqlError HSPARQL.SparqlError

appErrorToString :: AppError -> Text
appErrorToString (HTTPClientError hce) = httpClientErrorToString hce
appErrorToString (SparqlError sparqlError) = HSPARQL.sparqlErrorToText sparqlError

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
  putFileMetadata host blobUrl createdByUri maybeFileName size maybeTitle maybeDesc time mt = do
    logInfoN "Converting file metadata to RDF"
    -- generate a graph for the resource
    (url, rdf :: RDF TList) <- liftIO $ fileDataToRDF host blobUrl createdByUri maybeFileName size maybeTitle maybeDesc time mt

    logInfoN "Preparing to POST data to SPARQL endpoint"

    -- Post the graph to the SPARQL server via the Graph Store Protocol
    gspUri <- asks sparqlEndpoint
    mapError HTTPClientError (postToSPARQL gspUri rdf)

    pure url

  updateFileGraphWithContent fileUri blobUrl agentUri size time =
    mapError SparqlError $ HSPARQL.updateFileGraphWithContent fileUri blobUrl agentUri size time

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
