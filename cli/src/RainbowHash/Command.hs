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

import Options.Applicative (Parser, metavar, strArgument, long, short, help, subparser, command, info, progDesc, ParserInfo, fullDesc, header, flag')

import RainbowHash.CLI (putFileMoveOnError, watchDirectoryMoveOnError, uploadDirectoryMoveOnError)
import RainbowHash.CLI.Config (Config (..), fromBool)
import RainbowHash.App (runApp)
import System.Directory (doesDirectoryExist)

data Command
  = WatchDir WatchDirOptions
  -- merge the following since the option parser cannot determine which of the
  -- two the user intended without checking if the path is a file or directory
  -- during parsing which cannot be done since option parsing is pure.
  | Upload UploadOptions
  deriving (Show)

data WatchDirOptions = WatchDirOptions
  { dirToWatch :: FilePath
  , wdoDeleteAfterUpload :: Maybe Bool
  } deriving (Show)

data UploadOptions = UploadOptions
  { fileOrDirectory :: FilePath
  , uoDeleteAfterUpload :: Maybe Bool
  } deriving (Show)

deleteAfterFlag :: Parser Bool
deleteAfterFlag = flag' True (long "delete-after-upload" <> short 'd' <> help "Whether to delete the uploaded file")

keepFlag :: Parser Bool
keepFlag = flag' True (long "keep-after-upload" <> short 'k' <> help "Don not delete a file after upload")

deleteParser :: Parser (Maybe Bool)
deleteParser = optional $ deleteAfterFlag <|> keepFlag

watchCommand :: Parser Command
watchCommand = WatchDir <$>
  ( WatchDirOptions
    <$> strArgument (metavar "DIR" <> help "The directory to watch")
    <*> deleteParser)

uploadCommand :: Parser Command
uploadCommand = Upload <$>
  ( UploadOptions
    <$> strArgument (metavar "FILE-OR-DIR" <> help "The file or directory to upload")
    <*> deleteParser)

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

runCommand
  :: Config
  -> Command
  -> IO ()
runCommand config cmd = do
  eitherRes <- case cmd of
        WatchDir (WatchDirOptions dir shouldDelete) -> do
          let config' = config { deleteAction = maybe (deleteAction config) fromBool shouldDelete }
          runApp (watchDirectoryMoveOnError dir) config'
          
        Upload (UploadOptions fileOrDirectory' shouldDelete) -> do
          isDir <- doesDirectoryExist fileOrDirectory'
          let config' = config { deleteAction = maybe (deleteAction config) fromBool shouldDelete }
              app = if isDir
                then uploadDirectoryMoveOnError fileOrDirectory'
                else putFileMoveOnError fileOrDirectory'
          runApp app config'

  either print pure eitherRes
