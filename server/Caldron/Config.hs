{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Config
  ( Config(..)
  , StoredConfig(..)
  , getConfig
  ) where

import Protolude

import           Control.Monad.Logger (LogLevel(LevelInfo))
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.!=), (.=), object, withObject, withText)
import           Data.Default (Default(..))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as YAML
import qualified System.Directory as D
import qualified System.Environment as Env
import           System.FilePath ((</>), takeDirectory)
import           Text.URI (URI, mkURI, render)

import           Caldron.EmailAddress (EmailAddress)
import           RainbowHash.Logger (writeLog)

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
  } deriving (Show, Eq)

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
    scBlobStoreUrl <- o .:? "file-store-url"
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

getConfigFilePath :: IO FilePath
getConfigFilePath = (</> "server" </> "config.yaml") <$> D.getXdgDirectory D.XdgConfig "caldron"

getConfig :: IO StoredConfig
getConfig = do
  envConfig <- getConfigFromEnv
  maybeFileConfig <- getConfigFromFile
  let config = case maybeFileConfig of
        Just fileConfig -> mergeConfigs envConfig fileConfig
        Nothing -> envConfig
  if config == def
    then do
      writeLog LevelInfo "No configuration found. Creating default config file."
      writeStoredConfigToFile def
      pure def
    else pure config

writeStoredConfigToFile :: StoredConfig -> IO ()
writeStoredConfigToFile config = do
  configFilePath <- getConfigFilePath
  writeLog LevelInfo $ "Writing config to file " <> T.pack configFilePath
  YAML.encodeFile configFilePath config

getConfigFromEnv :: IO StoredConfig
getConfigFromEnv = do
  writeLog LevelInfo "Reading configuration from environment variables"
  blobStoreUrl <- readEnvURI "FILE_STORE_URL"
  sparqlEndpoint <- readEnvURI "SPARQL_URL"
  preferredHost <- readEnvText "PREFERRED_HOST"
  -- Note: WebID map is complex to parse from env vars, keeping file-based for now
  pure $ StoredConfig blobStoreUrl sparqlEndpoint preferredHost Map.empty
  where
    readEnvURI :: [Char] -> IO (Maybe URI)
    readEnvURI envVar = do
      maybeVal <- Env.lookupEnv envVar
      case maybeVal of
        Nothing -> pure Nothing
        Just val -> case mkURI (T.pack val) of
          Left _ -> do
            writeLog LevelInfo $ "Invalid URI in environment variable " <> T.pack envVar <> ": " <> T.pack val
            pure Nothing
          Right uri -> pure (Just uri)

    readEnvText :: [Char] -> IO (Maybe Text)
    readEnvText envVar = fmap T.pack <$> Env.lookupEnv envVar

mergeConfigs :: StoredConfig -> StoredConfig -> StoredConfig
mergeConfigs envConfig fileConfig = StoredConfig
  { scBlobStoreUrl = scBlobStoreUrl envConfig <|> scBlobStoreUrl fileConfig
  , scSparqlEndpoint = scSparqlEndpoint envConfig <|> scSparqlEndpoint fileConfig
  , scPreferredHost = scPreferredHost envConfig <|> scPreferredHost fileConfig
  , scWebIdMap = scWebIdMap fileConfig <> scWebIdMap envConfig  -- env vars take precedence for conflicts
  }

getConfigFromFile :: IO (Maybe StoredConfig)
getConfigFromFile = do
  configFilePath <- getConfigFilePath
  writeLog LevelInfo $ "Looking for configuration in file " <> T.pack configFilePath
  let configDir = takeDirectory configFilePath
  D.createDirectoryIfMissing True configDir
  eitherConfig <- YAML.decodeFileEither configFilePath
  pure $ fromRight Nothing eitherConfig
