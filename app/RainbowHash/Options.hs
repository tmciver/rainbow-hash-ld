{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RainbowHash.Options
  ( Options(..)
  , optionsParserInfo
  ) where

import           Protolude

import Numeric.Natural (Natural)
import           Options.Applicative (Parser, ReadM, showDefault, eitherReader, metavar, value, option, strOption, auto, long, short, help, helper, info, progDesc, ParserInfo, fullDesc, header)
import           Text.URI            (URI, mkURI)

data Options = Options
  { port           :: Natural
  , fileStoreUrl   :: URI
  , sparqlEndpoint :: URI
  , defaultHost :: Maybe Text
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
  <*> option uri
      ( long "file-store-url"
     <> help "URL of a rainbow-hash compatible file store."
     <> metavar "URL" )
  <*> option uri
      ( long "sparql-url"
     <> help "URL of the SPARQL endpoint."
     <> metavar "URL" )
  <*> optional (strOption
      ( long "default-host"
     <> help "Hostname to use if Host header is absent."
     <> value "example.com"
     <> showDefault
     <> metavar "HOST" ))

uri :: ReadM URI
uri = eitherReader $ first (const "Could not parse URL") . mkURI . toS

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper)
  ( fullDesc
    <> progDesc "A web-based file storage application utilizing linked data"
    <> header "rainbow-hash-ld - a linked data file storage application" )
