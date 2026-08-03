{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.String (String)
import qualified Data.Text as T
import qualified Data.Yaml as YAML
import qualified System.Directory as D
import qualified System.Environment as Env
import           Text.URI (URI, mkURI, render)

import qualified Data.Set            as Set

import           Caldron.EmailAddress (EmailAddress)
import           Caldron.Options (Options (..))
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
  , preferredHost  :: Maybe Text
  -- Bearer tokens accepted from trusted internal services
  , serviceTokens  :: Set Text
  }

resolveConfigFilePath :: Maybe FilePath -> IO FilePath
resolveConfigFilePath (Just path) = pure path
resolveConfigFilePath Nothing = do
  envPath <- Env.lookupEnv "CALDRON_CONFIG"
  case envPath of
    Just path -> pure path
    Nothing   -> pure "./config.yaml"

lookupEnvURI :: String -> IO (Maybe URI)
lookupEnvURI envVar = do
  maybeVal <- Env.lookupEnv envVar
  case maybeVal of
    Nothing  -> pure Nothing
    Just val -> case mkURI (T.pack val) of
      Left _ -> do
        writeLog LevelInfo $ "Invalid URI in environment variable " <> T.pack envVar <> ": " <> T.pack val
        pure Nothing
      Right uri -> pure (Just uri)

-- | Read the config file at the given path, returning Nothing if the file does
-- not exist or cannot be parsed.
readStoredConfig :: MonadIO m => FilePath -> m (Maybe StoredConfig)
readStoredConfig path = liftIO $ do
  exists <- D.doesFileExist path
  if exists
    then fromRight Nothing <$> YAML.decodeFileEither path
    else pure Nothing

getBlobStoreUrl
  :: ( MonadIO m
     , MonadError Text m
     )
  => Options
  -> Maybe StoredConfig
  -> m URI
getBlobStoreUrl Options{fileStoreUrl = mOptUri} mStoredConfig = do
  mEnvUri <- liftIO $ lookupEnvURI "FILE_STORE_URL"
  let result = mOptUri
           <|> mEnvUri
           <|> (mStoredConfig >>= scBlobStoreUrl)
  maybe (throwError "Missing blob store URL. Provide with --file-store-url, FILE_STORE_URL env var, or in config file.") pure result

getSparqlEndpoint
  :: ( MonadIO m
     , MonadError Text m
     )
  => Options
  -> Maybe StoredConfig
  -> m URI
getSparqlEndpoint Options{sparqlEndpoint = mOptUri} mStoredConfig = do
  mEnvUri <- liftIO $ lookupEnvURI "SPARQL_URL"
  let result = mOptUri
           <|> mEnvUri
           <|> (mStoredConfig >>= scSparqlEndpoint)
  maybe (throwError "Missing SPARQL endpoint URL. Provide with --sparql-url, SPARQL_URL env var, or in config file.") pure result

getServiceTokens :: IO (Set Text)
getServiceTokens = do
  mVal <- Env.lookupEnv "SERVICE_TOKENS"
  pure $ case mVal of
    Nothing  -> Set.empty
    Just val -> Set.fromList . filter (not . T.null) . T.splitOn "," . T.pack $ val

getConfig
  :: ( MonadIO m
     , MonadError Text m
     )
  => Options
  -> m Config
getConfig opts = do
  configPath <- liftIO $ resolveConfigFilePath (configFile opts)
  mStoredConfig <- readStoredConfig configPath
  tokens <- liftIO getServiceTokens
  Config
    <$> getBlobStoreUrl opts mStoredConfig
    <*> getSparqlEndpoint opts mStoredConfig
    <*> pure (maybe Map.empty scWebIdMap mStoredConfig)
    <*> pure (defaultHost opts <|> (mStoredConfig >>= scPreferredHost))
    <*> pure tokens

writeStoredConfigToFile :: FilePath -> StoredConfig -> IO ()
writeStoredConfigToFile configFilePath config = do
  writeLog LevelInfo $ "Writing config to file " <> T.pack configFilePath
  YAML.encodeFile configFilePath config
