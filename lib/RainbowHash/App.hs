{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.App where

import Protolude

import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr)
--import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe (fromJust)
import qualified Data.Time.Clock as Time
import Network.URL (importURL, URL)

import RainbowHash.LinkedData

newtype Env = Env { blobStoreUrl :: URL }

newtype AppM a = AppM (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runApp :: AppM a -> Env -> IO a
runApp (AppM rdr) env = runReaderT rdr env

instance FilePut AppM FilePath where
  putFileInStore fp = do
    blobStoreUrl' <- asks blobStoreUrl
    putStrLn $ "Putting " <> fp <> " in the store at " <> show blobStoreUrl'
    fp
      & ("file://" <>)
      & importURL
      & fromJust
      & pure

instance MetadataPut AppM where
  putFileMetadata url _ _ = do
    --putStrLn ("Putting file metadata." :: Text)
    pure url

instance MediaTypeDiscover AppM FilePath where
  getMediaType _ = do
    putStrLn ("Discovering media type." :: Text)
    pure $ MediaType "application/octet-stream" ""

instance Time AppM where
  getCurrentTime = do
    --putStrLn ("Getting current time." :: Text)
    liftIO Time.getCurrentTime

instance MonadLogger AppM where
  monadLoggerLog _ _ _ = liftIO . putStrLn . fromLogStr . toLogStr
