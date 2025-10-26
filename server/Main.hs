{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude

import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (execParser)
import           Text.URI                 (render)

import           RainbowHash.Config       (Config (Config))
import           RainbowHash.Options      (Options (..), optionsParserInfo)
import           RainbowHash.Server       (app)

configToText :: Config -> Text
configToText (Config fileStoreUrl' sparqlEndpoint' defaultHost') =
  "Configuration:\n"
  <> "  File store URL: " <> render fileStoreUrl' <> "\n"
  <> "  SPARQL Endpoint: " <> render sparqlEndpoint' <> "\n"
  <> "  Default host: " <> fromMaybe "Not set" defaultHost'

main :: IO ()
main = do
  Options{..} <- execParser optionsParserInfo
  let config = Config fileStoreUrl sparqlEndpoint defaultHost
      port' = fromIntegral port
  putStrLn $ configToText config
  putStrLn $ "Caldron running on port " <> (show port' :: Text)
  run port' (app config)
