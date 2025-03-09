{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.App
  ( AppM
  , AppError(..)
  , Env(..)
  , runApp
  , appErrorToString
  ) where

import Protolude

import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr)
import Data.RDF (RDF, TList)
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import System.FilePath (takeFileName)
import Text.URI (URI)

import RainbowHash.LinkedData
import RainbowHash.HTTPClient as HTTPClient (mapError, putFile, httpClientErrorToString, postToSPARQL, HTTPClientError)
import RainbowHash.MediaTypeDiscover (discoverMediaTypeFP)
import RainbowHash.RDF4H (fileDataToRDF)

data Env = Env
  { blobStoreUrl :: URI
  , sparqlEndpoint :: URI
  }

newtype AppError = HTTPClientError HTTPClientError
  deriving (Show)

appErrorToString :: AppError -> Text
appErrorToString (HTTPClientError hce) = httpClientErrorToString hce

newtype AppM a = AppM (ExceptT AppError (ReaderT Env IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError AppError, MonadMask, MonadCatch, MonadThrow)

runApp :: AppM a -> Env -> IO (Either AppError a)
runApp (AppM except) = runReaderT (runExceptT except)

instance FilePut AppM FilePath where
  putFileInStore fp = do
    blobStoreUrl' <- asks blobStoreUrl
    mapError HTTPClientError (HTTPClient.putFile blobStoreUrl' fp)

instance MetadataPut AppM where
  putFileMetadata blobUrl createdByUri maybeFileName time mt = do
    -- generate a graph for the resource
    (url, rdf :: RDF TList) <- liftIO $ fileDataToRDF blobUrl createdByUri maybeFileName time mt

    -- Post the graph to the SPARQL server via the Graph Store Protocol
    gspUri <- asks sparqlEndpoint
    mapError HTTPClientError (postToSPARQL gspUri rdf)

    pure url

instance MediaTypeDiscover AppM FilePath where
  getMediaType = liftIO . discoverMediaTypeFP

instance FileNameGet AppM FilePath where
  getFileName = pure . Just . T.pack . takeFileName

instance Time AppM where
  getCurrentTime = liftIO Time.getCurrentTime

instance MonadLogger AppM where
  monadLoggerLog _ _ _ = liftIO . putStrLn . fromLogStr . toLogStr
