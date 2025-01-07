{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.App where

import Protolude

import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr)
import Data.Maybe (fromJust)
import qualified Data.Time.Clock as Time
import Network.URL (importURL)

import RainbowHash.LinkedData

newtype AppM a = AppM (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: AppM a -> IO a
runApp (AppM x) = x

instance FilePut AppM FilePath where
  putFileInStore fp = do
    --putStrLn $ "Putting " <> fp <> " in the store."
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
