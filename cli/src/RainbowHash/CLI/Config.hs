{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.CLI.Config
  ( StoredConfig(..)
  , Config(..)
  , DeleteAction(..)
  , fromBool
  , getStoredConfig
  ) where

import Protolude

import Data.Aeson (ToJSON (..), object, (.=), FromJSON (..), withObject, (.:), (.!=), (.:?))
import GHC.Natural (Natural)
import Text.URI (URI (..), mkURI, Authority (..), unRText)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified System.Directory as D
import System.FilePath ((</>), takeDirectory)
import qualified Data.Yaml as YAML
import Data.Default
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad.Logger (LogLevel(LevelInfo))

import RainbowHash.EmailAddress (EmailAddress)
import RainbowHash.Logger (writeLog)

data DeleteAction
  = Delete
  | NoDelete
  deriving (Eq, Show)

fromBool :: Bool -> DeleteAction
fromBool True = Delete
fromBool False = NoDelete

toBool :: DeleteAction -> Bool
toBool Delete = True
toBool NoDelete = False

data StoredConfig = StoredConfig
  { scServerUri :: Maybe URI
  , scDeleteAction :: DeleteAction
  , scExtensionsToIgnore :: Set Text
  , scEmailMap :: Map Text EmailAddress
  , scCertPath :: Maybe FilePath
  , scKeyPath :: Maybe FilePath
  } deriving (Show)

data Config = Config
  { serverUri :: URI
  , deleteAction :: DeleteAction
  , extensionsToIgnore :: Set Text
  , emailMap :: Map Text EmailAddress
  , certPath :: FilePath
  , keyPath :: FilePath
  } deriving (Show)

instance ToJSON StoredConfig where
  toJSON StoredConfig {..} =
    let serverObj = case scServerUri of
          Nothing -> []
          Just uri ->
            let authority = either (panic "Could not get host from config") identity . uriAuthority $ uri
                host = unRText . authHost $ authority
                port = fromJust . authPort $ authority
            in [ "server" .= object [ "host" .= host, "port" .= port ] ]
        deleteObj = [ "delete-uploaded-file" .= toBool scDeleteAction ]
        extensionsObj = [ "extensions-to-ignore" .= scExtensionsToIgnore ]
        emailMapObj = [ "email-webid-map" .= toJSON scEmailMap ]
        certPathObj = maybe [] (\p -> [ "cert-path" .= p ]) scCertPath
        keyPathObj = maybe [] (\p -> [ "key-path" .= p ]) scKeyPath
    in object $ serverObj <> deleteObj <> extensionsObj <> emailMapObj <> certPathObj <> keyPathObj

instance FromJSON StoredConfig where
  parseJSON = withObject "StoredConfig" $ \o -> do
    mServer <- o .:? "server"
    scServerUri <- for mServer $ \server -> do
      host <- server .: "host"
      port <- server .: "port"
      pure $ getURI host port
    delete <- o .:? "delete-uploaded-file" .!= False
    scExtensionsToIgnore <- o .:? "extensions-to-ignore" .!= Set.empty
    scEmailMap <- o .:? "email-webid-map" .!= Map.empty
    scCertPath <- o .:? "cert-path"
    scKeyPath <- o .:? "key-path"
    let scDeleteAction = fromBool delete
    pure StoredConfig {..}

instance Default StoredConfig where
  def = StoredConfig
    { scServerUri = Nothing
    , scDeleteAction = NoDelete
    , scExtensionsToIgnore = Set.empty
    , scEmailMap = Map.empty
    , scCertPath = Nothing
    , scKeyPath = Nothing
    }

getURI :: Text -> Natural -> URI
getURI host port =
  fromJust $ mkURI ("http://" <> host <> ":" <> show port)

getConfigFile :: IO FilePath
getConfigFile = (</> "cli" </> "config.yaml") <$> D.getXdgDirectory D.XdgConfig "caldron"

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
