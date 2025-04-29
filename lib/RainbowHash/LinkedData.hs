{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.LinkedData
  ( putFile
  , FilePut(..)
  , FileGet(..)
  , MetadataPut(..)
  , MediaTypeDiscover(..)
  , FileNameGet(..)
  , Time(..)
  ) where

import Protolude

import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Media (MediaType, renderHeader)
import Text.URI (URI, render)

import RainbowHash.File (File)

class Monad m => FilePut m v where
  putFileInStore :: v -> m URI

class Monad m => FileGet m where
  getFile :: URI -> m (Maybe File)
  getRecentFiles :: m [File]

class Monad m => MetadataPut m where
  putFileMetadata
    :: URI -- ^URI of file data in blob storage
    -> URI -- ^URI of agent creating the file
    -> Maybe Text -- ^file name. May be unavailable if client calls putFile on ByteString.
    -> Maybe Text -- ^title
    -> Maybe Text -- ^description
    -> UTCTime -- ^file creation time
    -> MediaType
    -> m URI

class Monad m => MediaTypeDiscover m v where
  getMediaType :: v -> m MediaType

class Monad m => FileNameGet m v where
  getFileName :: v -> m (Maybe Text)

class Monad m => Time m where
  getCurrentTime :: m UTCTime

putFile
  :: ( FilePut m v
     , MetadataPut m
     , MediaTypeDiscover m v
     , FileNameGet m v
     , Time m
     , MonadLogger m
     )
  => v
  -> URI -- ^URI of agent putting the file
  -> Maybe Text -- ^filename
  -> Maybe Text -- ^title
  -> Maybe Text -- ^description
  -> Maybe MediaType
  -> m URI
putFile v createdByUri maybeFileName maybeTitle maybeDesc maybeMT = do

  -- Get the current time
  t <- getCurrentTime

  -- Use given media type or discover what it is.
  mt <- maybe (getMediaType v) pure maybeMT

  -- Use the given filename or get it from v
  maybeFileName' <- case maybeFileName of
    Just fn -> pure $ Just fn
    Nothing -> getFileName v

  -- Add file to blob store.
  blobUrl <- putFileInStore v

  -- Add the metadata to the linked data store.
  fileUrl <- putFileMetadata blobUrl createdByUri maybeFileName' maybeTitle maybeDesc t mt

  logPutFile fileUrl blobUrl t mt

  pure fileUrl

logPutFile
  :: MonadLogger m
  => URI
  -> URI
  -> UTCTime
  -> MediaType
  -> m ()
logPutFile fileUrl blobUrl t mt =
  logInfoN
    $  "Created file object " <> toS (render fileUrl)
    <> " with blob URI " <> toS (render blobUrl)
    <> " and media type " <> (T.decodeUtf8 $ renderHeader mt)
    <> " at " <> show t
