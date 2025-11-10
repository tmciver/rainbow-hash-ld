{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RainbowHash.CLI
  ( HttpRead(..)
  , HttpWrite(..)
  , FileSystemRead(..)
  , FileSystemWrite(..)
  , DirectoryWatch(..)
  , AppError(..)
  , putFile
  , putFileMoveOnError
  , watchDirectoryMoveOnError
  , uploadDirectoryMoveOnError
  ) where

import Protolude hiding (readFile)

import qualified Data.Map as Map
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import Control.Monad.Logger (MonadLogger, logInfoN, logErrorN)

import RainbowHash (calcHash, Hash)
import System.FilePath ((</>), takeDirectory)
import RainbowHash.CLI.Config (DeleteAction(..))
import RainbowHash.EmailAddress (EmailAddress(..))
import qualified Data.Text as T

data AppError
  = EmailNotFound Text
  | PostError Text
  deriving (Eq, Show)

class MonadError AppError m => HttpWrite m where
  postFile :: FilePath -> EmailAddress -> m ()

class HttpRead m where
  doesFileExistInStore :: Hash -> m Bool

class Monad m => FileSystemRead m where
  readFile :: FilePath -> m ByteString
  listDirectory :: FilePath -> m (OSet FilePath)
  doesFileExist :: FilePath -> m Bool
  getFileOwner :: FilePath -> m Text -- Can this fail?

class Monad m => FileSystemWrite m where
  createDirectory :: FilePath -> m ()
  moveToDirectory
    :: FilePath -- ^Path of file to move
    -> FilePath -- ^Directory to move it to
    -> m ()
  deleteFile :: FilePath -> m ()

class Monad m => DirectoryWatch m where
  watchDirectory
    :: FilePath -- ^Directory to watch
    -> (FilePath -> m ()) -- ^Action to run. It gets passed the full path to a file that was added.
    -> m ()

watchDirectoryMoveOnError
  :: ( DirectoryWatch m
     , FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , HasField "deleteAction" env DeleteAction
     , HasField "extensionsToIgnore" env (Set Text)
     , HasField "emailMap" env (Map Text EmailAddress)
     , MonadReader env m
     , MonadLogger m
     )
  => FilePath -- ^Directory to watch
  -> m ()
watchDirectoryMoveOnError dir =
  watchDirectory dir putFileMoveOnError

shouldBeIgnored
  :: ( HasField "extensionsToIgnore" env (Set Text)
     , MonadReader env m
     )
  => FilePath
  -> m Bool
shouldBeIgnored fp = do
  exts <- asks (getField @"extensionsToIgnore")
  pure $ any (`T.isSuffixOf` T.pack fp) exts

getUserEmail
  :: ( HasField "emailMap" env (Map Text EmailAddress)
     , MonadReader env m
     )
  => Text -- ^username
  -> m (Maybe EmailAddress)
getUserEmail username = Map.lookup username <$> asks (getField @"emailMap")

fromJustM
  :: Monad m
  => m (Maybe a)
  -> m a
  -> m a
fromJustM mm onNothing = do
  mx <- mm
  case mx of
    Nothing -> onNothing
    Just x -> pure x

putFile
  :: ( FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , HasField "deleteAction" env DeleteAction
     , HasField "extensionsToIgnore" env (Set Text)
     , HasField "emailMap" env (Map Text EmailAddress)
     , MonadReader env m
     , MonadLogger m
     )
  => FilePath -- ^the file to upload
  -> m ()
putFile filePath = do
  logInfoN ""
  logInfoN $ "Attempting to Upload file " <> T.pack filePath

  -- Check if this file should be ignored.
  shouldBeIgnored' <- shouldBeIgnored filePath
  if shouldBeIgnored'
    then logInfoN $ "Ignoring " <> T.pack filePath
    else  do
      -- Calculate the hash of the file's content.
      bs <- readFile filePath

      -- Find the email configured for the file owner
      username <- getFileOwner filePath
      userEmail <- fromJustM (getUserEmail username) (throwError $ EmailNotFound username)
      logInfoN $ "Found email " <> getEmailAddress userEmail <> " for user " <> username

      -- Post the file
      postFile filePath userEmail

      -- Delete the local file if configured.
      deleteAction <- asks (getField @"deleteAction")
      case deleteAction of
        Delete -> deleteFile filePath
        NoDelete -> pure ()

putFileMoveOnError
  :: ( FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , HasField "deleteAction" env DeleteAction
     , HasField "extensionsToIgnore" env (Set Text)
     , HasField "emailMap" env (Map Text EmailAddress)
     , MonadReader env m
     , MonadLogger m
     )
  => FilePath -- ^Directory to move file to on error ->
  --SystemFile -- ^file to upload
  -> m ()
putFileMoveOnError filePath = do
  let dir = takeDirectory filePath
      errorDir = dir </> "upload-errors"
  putFile filePath `catchError` (\case
                                    (PostError _) -> do
                                      createDirectory errorDir
                                      moveToDirectory filePath errorDir
                                    e -> logErrorN $ "Got an unexpected error: " <> show e
                                )

uploadDirectoryMoveOnError
  :: ( FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , HasField "deleteAction" env DeleteAction
     , HasField "extensionsToIgnore" env (Set Text)
     , HasField "emailMap" env (Map Text EmailAddress)
     , MonadReader env m
     , MonadLogger m
     )
  => FilePath -- ^the directory whose contents will be uploaded.
  -> m ()
uploadDirectoryMoveOnError dir = do
  listDirectory dir
    <&> (fmap (dir </>) . OSet.toAscList)
    >>= filterM doesFileExist
    >>= traverse_ putFileMoveOnError
