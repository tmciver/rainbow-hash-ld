{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude

import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (execParser)
import           Text.URI                 (render)

import           RainbowHash.Config       (Config (Config), getStoredConfig)
import           RainbowHash.Options      (Options (..), optionsParserInfo, optionsToConfig)
import           RainbowHash.Server       (app)

configToText :: Config -> Text
configToText (Config fileStoreUrl' sparqlEndpoint' webIdMap defaultHost') =
  "Configuration:\n"
  <> "  File store URL: " <> render fileStoreUrl' <> "\n"
  <> "  SPARQL Endpoint: " <> render sparqlEndpoint' <> "\n"
  <> "  Default host: " <> fromMaybe "Not set" defaultHost' <> "\n"
  <> "   WebID mapping: " <> show webIdMap

main :: IO ()
main = do
  opts@Options{..} <- execParser optionsParserInfo
  storedConfig <- getStoredConfig
  let eitherConfig = optionsToConfig opts storedConfig
      port' = fromIntegral port
  case eitherConfig of
    Left err -> putStrLn err
    Right config -> do
      putStrLn $ configToText config
      putStrLn $ "Caldron running on port " <> (show port' :: Text)
      run port' (app config)
