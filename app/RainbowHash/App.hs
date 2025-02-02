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

import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr)
import qualified Data.Time.Clock as Time
import Network.URL (URL)

import RainbowHash.LinkedData
import RainbowHash.HTTPClient as HTTPClient (mapError, putFile, httpClientErrorToString, HTTPClientError)
import RainbowHash.MediaTypeDiscover (discoverMediaTypeFP)

data Env = Env
  { blobStoreUrl :: URL
  , sparqlEndpoint :: URL
  }

newtype AppError = HTTPClientError HTTPClientError
  deriving (Show)

appErrorToString :: AppError -> Text
appErrorToString (HTTPClientError hce) = httpClientErrorToString hce

newtype AppM a = AppM (ExceptT AppError (ReaderT Env IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError AppError)

runApp :: AppM a -> Env -> IO (Either AppError a)
runApp (AppM except) = runReaderT (runExceptT except)

instance FilePut AppM FilePath where
  putFileInStore fp = do
    blobStoreUrl' <- asks blobStoreUrl
    mapError HTTPClientError (HTTPClient.putFile blobStoreUrl' fp)

instance MetadataPut AppM where
  putFileMetadata url _ _ = do
    --putStrLn ("Putting file metadata." :: Text)
    pure url

instance MediaTypeDiscover AppM FilePath where
  getMediaType = liftIO . discoverMediaTypeFP

instance Time AppM where
  getCurrentTime = do
    --putStrLn ("Getting current time." :: Text)
    liftIO Time.getCurrentTime

instance MonadLogger AppM where
  monadLoggerLog _ _ _ = liftIO . putStrLn . fromLogStr . toLogStr
