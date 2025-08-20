{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude

import           Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser)

import           RainbowHash.Config       (Config (..))
import           RainbowHash.Options      (Options (..), optionsParserInfo)
import           RainbowHash.Server       (app)

main :: IO ()
main = do
  Options{..} <- execParser optionsParserInfo
  let config = Config fileStoreUrl sparqlEndpoint
      port' = fromIntegral port
  run port' (app config)
