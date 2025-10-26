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
  , HttpError(..)
  , putFile
  , putFileMoveOnError
  , watchDirectoryMoveOnError
  , uploadDirectoryMoveOnError
  ) where

import Protolude hiding (readFile)

import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import Control.Monad.Logger (MonadLogger, logInfoN)

import RainbowHash (calcHash, Hash)
import System.FilePath ((</>), takeDirectory)
import RainbowHash.CLI.Config (DeleteAction(..))
import qualified Data.Text as T

newtype HttpError = PostError Text
  deriving (Eq, Show)

class MonadError HttpError m => HttpWrite m where
  postFile :: FilePath -> m ()

class HttpRead m where
  doesFileExistInStore :: Hash -> m Bool

class Monad m => FileSystemRead m where
  readFile :: FilePath -> m ByteString
  listDirectory :: FilePath -> m (OSet FilePath)
  doesFileExist :: FilePath -> m Bool

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

putFile
  :: ( FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , HasField "deleteAction" env DeleteAction
     , HasField "extensionsToIgnore" env (Set Text)
     , MonadReader env m
     , MonadLogger m
     )
  => FilePath -- ^the file to upload
  -> m ()
putFile fp = do
  logInfoN ""
  logInfoN $ "Attempting to Upload file " <> T.pack fp

  -- Check if this file should be ignored.
  shouldBeIgnored' <- shouldBeIgnored fp
  if shouldBeIgnored'
    then logInfoN $ "Ignoring " <> T.pack fp
    else  do
      -- Calculate the hash of the file's content.
      bs <- readFile fp
      let hash' = calcHash bs

      -- Only upload the file if it doesn't exist on the server.
      fileExists <- doesFileExistInStore hash'
      if fileExists
        then logInfoN ("File exists on server; not uploading." :: Text)
        else postFile fp

      -- Delete the local file if configured.
      deleteAction <- asks (getField @"deleteAction")
      case deleteAction of
        Delete -> deleteFile fp
        NoDelete -> pure ()

putFileMoveOnError
  :: ( FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , HasField "deleteAction" env DeleteAction
     , HasField "extensionsToIgnore" env (Set Text)
     , MonadReader env m
     , MonadLogger m
     )
  => --FilePath -- ^Directory to move file to on error ->
  FilePath -- ^Path to file to upload
  -> m ()
putFileMoveOnError fp = do
  let dir = takeDirectory fp
      errorDir = dir </> "upload-errors"
  putFile fp `catchError` (\(PostError _) -> do
                              createDirectory errorDir
                              moveToDirectory fp errorDir)

uploadDirectoryMoveOnError
  :: ( FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , HasField "deleteAction" env DeleteAction
     , HasField "extensionsToIgnore" env (Set Text)
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
