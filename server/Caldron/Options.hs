{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Caldron.Options
  ( Options(..)
  , optionsParserInfo
  , optionsToConfig
  ) where

import           Protolude

import           Numeric.Natural     (Natural)
import           Options.Applicative (Parser, ReadM, auto, eitherReader, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, showDefault, strOption, value, ParserInfo)
import           Text.URI            (URI, mkURI)

import Caldron.Config (Config (..), StoredConfig (..))

data Options = Options
  { port           :: Natural
  , fileStoreUrl   :: Maybe URI
  , sparqlEndpoint :: Maybe URI
  , defaultHost    :: Maybe Text
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> option auto
      ( long "port"
     <> short 'p'
     <> help "Port to listen on"
     <> showDefault
     <> value 80
     <> metavar "INT" )
  <*> optional (option uri
      ( long "file-store-url"
     <> help "URL of a rainbow-hash compatible file store."
     <> metavar "URL" ))
  <*> optional (option uri
      ( long "sparql-url"
     <> help "URL of the SPARQL endpoint."
     <> metavar "URL" ))
  <*> optional (strOption
      ( long "default-host"
     <> help "Hostname to use if Host header is absent."
     <> metavar "HOST" ))

uri :: ReadM URI
uri = eitherReader $ first (const "Could not parse URL") . mkURI . toS

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper)
  ( fullDesc
    <> progDesc "A web-based file storage application utilizing linked data"
    <> header "Caldron - a linked data file storage application" )

optionsToConfig
  :: Options
  -> StoredConfig
  -> Either Text Config
optionsToConfig Options{..} StoredConfig{..} = do
  bsu <- maybeToRight "Missing blob store URL. Provide with --file-store-url or in config file."
    $ fileStoreUrl <|> scBlobStoreUrl
  spe <- maybeToRight "Missing SPARQL endpoint URL. Provide with --sparql-url or in config file."
    $ sparqlEndpoint <|> scSparqlEndpoint
  let ph = defaultHost <|> scPreferredHost
      wim = scWebIdMap
  pure $ Config bsu spe wim ph
