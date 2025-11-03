{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.Command
  ( Command(..)
  , runCommand
  , options
  ) where

import Protolude
--import Control.Error (note)

import Options.Applicative (Parser, metavar, strArgument, long, short, help, subparser, command, info, progDesc, ParserInfo, fullDesc, header, flag', option, eitherReader, ReadM, strOption)
import Text.URI (URI)
import qualified Text.URI as URI
import qualified Data.Text as T

import RainbowHash.CLI (putFileMoveOnError, watchDirectoryMoveOnError, uploadDirectoryMoveOnError)
import RainbowHash.CLI.Config (StoredConfig (..), Config (..), fromBool)
import RainbowHash.App (runApp)
import System.Directory (doesDirectoryExist)

data Command
  = WatchDir WatchDirOptions
  -- merge the following since the option parser cannot determine which of the
  -- two the user intended without checking if the path is a file or directory
  -- during parsing which cannot be done since option parsing is pure.
  | Upload UploadOptions
  deriving (Show)

data CommonOptions = CommonOptions
  { coDeleteAfterUpload :: Maybe Bool
  , coServerUri :: Maybe URI
  , coCertPath :: Maybe FilePath
  , coKeyPath :: Maybe FilePath
  } deriving (Show)

data WatchDirOptions = WatchDirOptions
  { dirToWatch :: FilePath
  , wdoCommon :: CommonOptions
  } deriving (Show)

data UploadOptions = UploadOptions
  { fileOrDirectory :: FilePath
  , uoCommon :: CommonOptions
  } deriving (Show)

deleteAfterFlag :: Parser Bool
deleteAfterFlag = flag' True (long "delete-after-upload" <> short 'd' <> help "Whether to delete the uploaded file")

keepFlag :: Parser Bool
keepFlag = flag' True (long "keep-after-upload" <> short 'k' <> help "Don not delete a file after upload")

deleteParser :: Parser (Maybe Bool)
deleteParser = optional $ deleteAfterFlag <|> keepFlag

uriReader :: ReadM URI
uriReader = eitherReader $ \s -> case URI.mkURI (T.pack s) of
  Just uri -> Right uri
  Nothing -> Left $ "Could not parse URI: " <> s

commonOptionsParser :: Parser CommonOptions
commonOptionsParser = CommonOptions
  <$> deleteParser
  <*> optional (option uriReader (long "server-uri" <> help "The URI of the rainbow-hash server."))
  <*> optional (strOption (long "cert-path" <> help "Path to the X509 certificate file."))
  <*> optional (strOption (long "key-path" <> help "Path to the X509 key file."))

watchCommand :: Parser Command
watchCommand = WatchDir <$>
  ( WatchDirOptions
    <$> strArgument (metavar "DIR" <> help "The directory to watch")
    <*> commonOptionsParser)

uploadCommand :: Parser Command
uploadCommand = Upload <$>
  ( UploadOptions
    <$> strArgument (metavar "FILE-OR-DIR" <> help "The file or directory to upload")
    <*> commonOptionsParser)

commandParser :: Parser Command
commandParser = subparser
  ( command "watch" (info watchCommand (progDesc "Watch a given directory and upload files that are added to it. Does not upload existing files."))
 <> command "upload" (info uploadCommand (progDesc "Upload the given file or the files in the given directory."))
  )

options :: ParserInfo Command
options = info commandParser
  ( fullDesc
 <> progDesc "A command-line interface for a rainbow-hash server."
 <> header "A header for the CLI for rainbow-hash."
  )

getCommonOptions :: Command -> CommonOptions
getCommonOptions (WatchDir (WatchDirOptions _ common)) = common
getCommonOptions (Upload (UploadOptions _ common)) = common

mkConfig
  :: Command
  -> StoredConfig
  -> Either Text Config
mkConfig cmd StoredConfig{..} = do
  let CommonOptions{..} = getCommonOptions cmd
  serverUri <- note "Server URI is not specified." $ coServerUri <|> scServerUri
  certPath <- note "Certificate path is not specified." $ coCertPath <|> scCertPath
  keyPath <- note "Key path is not specified." $ coKeyPath <|> scKeyPath
  let deleteAction = maybe scDeleteAction fromBool coDeleteAfterUpload
      extensionsToIgnore = scExtensionsToIgnore
      emailMap = scEmailMap
  pure Config {..}

runCommand
  :: StoredConfig
  -> Command
  -> IO ()
runCommand storedConfig cmd =
  case mkConfig cmd storedConfig of
    Left err -> print err
    Right config -> do
      eitherRes <- case cmd of
            WatchDir (WatchDirOptions dir _) ->
              runApp (watchDirectoryMoveOnError dir) config

            Upload (UploadOptions fileOrDirectory' _) -> do
              isDir <- doesDirectoryExist fileOrDirectory'
              let app = if isDir
                    then uploadDirectoryMoveOnError fileOrDirectory'
                    else putFileMoveOnError fileOrDirectory'
              runApp app config

      either print pure eitherRes
