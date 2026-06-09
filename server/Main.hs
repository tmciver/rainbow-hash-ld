{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude

import           System.IO                (hSetBuffering, stdout, stderr, BufferMode (LineBuffering))
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (execParser)
import           Text.URI                 (render)

import           Caldron.Config       (Config (Config), getConfig)
import           Caldron.Options      (Options (..), optionsParserInfo)
import           Caldron.Server       (app)

configToText :: Config -> Text
configToText (Config fileStoreUrl' sparqlEndpoint' webIdMap defaultHost') =
  "Configuration:\n"
  <> "  File store URL: " <> render fileStoreUrl' <> "\n"
  <> "  SPARQL Endpoint: " <> render sparqlEndpoint' <> "\n"
  <> "  Default host: " <> fromMaybe "Not set" defaultHost' <> "\n"
  <> "  WebID mapping: " <> show webIdMap

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  opts <- execParser optionsParserInfo
  eitherConfig <- runExceptT $ getConfig opts
  let port' = fromIntegral $ port opts
  case eitherConfig of
    Left err -> putStrLn err
    Right config -> do
      putStrLn $ configToText config
      putStrLn $ "Caldron running on port " <> (show port' :: Text)
      run port' (app config)
