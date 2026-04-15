{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Config
  ( Config(..)
  , StoredConfig(..)
  , getStoredConfig
  ) where

import Protolude

import           Control.Monad.Logger (LogLevel(LevelInfo))
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.!=), (.=), object, withObject, withText)
import           Data.Default (Default(..))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as YAML
import qualified System.Directory as D
import           System.FilePath ((</>), takeDirectory)
import           Text.URI (URI, mkURI, render)

import           Caldron.EmailAddress (EmailAddress)
import           Caldron.Logger (writeLog)

instance ToJSON URI where
  toJSON = toJSON . render

instance FromJSON URI where
  parseJSON = withText "URI" $ \t ->
    either (panic . show) pure . mkURI $ t

data StoredConfig = StoredConfig
  { scBlobStoreUrl   :: Maybe URI
  , scSparqlEndpoint :: Maybe URI
  , scPreferredHost  :: Maybe Text
  , scWebIdMap       :: Map EmailAddress URI
  } deriving (Show)

instance Default StoredConfig where
  def = StoredConfig
    { scBlobStoreUrl = Nothing
    , scSparqlEndpoint = Nothing
    , scPreferredHost = Nothing
    , scWebIdMap = Map.empty
    }

instance ToJSON StoredConfig where
  toJSON StoredConfig{..} =
    let blobStoreUrlObj = maybe [] (\u -> [ "blob-store-url" .= u ]) scBlobStoreUrl
        sparqlEndpointObj = maybe [] (\u -> [ "sparql-endpoint" .= u ]) scSparqlEndpoint
        preferredHostObj = maybe [] (\h -> [ "preferred-host" .= h ]) scPreferredHost
        webIdMapObj = [ "webid-map" .= scWebIdMap ]
    in object $ blobStoreUrlObj <> sparqlEndpointObj <> preferredHostObj <> webIdMapObj

instance FromJSON StoredConfig where
  parseJSON = withObject "StoredConfig" $ \o -> do
    scBlobStoreUrl <- o .:? "blob-store-url"
    scSparqlEndpoint <- o .:? "sparql-endpoint"
    scPreferredHost <- o .:? "preferred-host"
    scWebIdMap <- o .:? "webid-map" .!= Map.empty
    pure StoredConfig {..}

data Config = Config
  { blobStoreUrl   :: URI
  , sparqlEndpoint :: URI
  , webIdMap       :: Map EmailAddress URI
  -- If present, use the configured host over that provided in the Host header
  , preferredHost :: Maybe Text
  }

getConfigFile :: IO FilePath
getConfigFile = (</> "server" </> "config.yaml") <$> D.getXdgDirectory D.XdgConfig "caldron"

getStoredConfig :: IO StoredConfig
getStoredConfig = do
  maybeConfig <- getStoredConfigFromFile
  case maybeConfig of
    Just config -> pure config
    Nothing -> do
      writeLog LevelInfo "Unable to read configuration."
      writeStoredConfigToFile def
      pure def

writeStoredConfigToFile :: StoredConfig -> IO ()
writeStoredConfigToFile config = do
  configFile <- getConfigFile
  writeLog LevelInfo $ "Writing config to file " <> T.pack configFile
  YAML.encodeFile configFile config

getStoredConfigFromFile :: IO (Maybe StoredConfig)
getStoredConfigFromFile = do
  configFile <- getConfigFile
  writeLog LevelInfo $ "Looking for configuration in file " <> T.pack configFile
  let configDir = takeDirectory configFile
  D.createDirectoryIfMissing True configDir
  eitherConfig <- YAML.decodeFileEither configFile
  pure $ fromRight Nothing eitherConfig
