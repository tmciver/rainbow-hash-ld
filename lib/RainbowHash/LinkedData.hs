{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.LinkedData
  ( putFile
  , FilePut(..)
  , MetadataPut(..)
  , MediaTypeDiscover(..)
  , Time(..)
  ) where

import Protolude

import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Time.Clock (UTCTime)
import Text.URI (URI, render)

import RainbowHash.MediaType

class Monad m => FilePut m v where
  putFileInStore :: v -> m URI

class Monad m => MetadataPut m where
  putFileMetadata
    :: URI -- ^URI of file data in blob storage
    -> Text -- ^file name
    -> UTCTime -- ^file creation time
    -> MediaType
    -> m URI

class Monad m => MediaTypeDiscover m v where
  getMediaType :: v -> m MediaType

class Monad m => Time m where
  getCurrentTime :: m UTCTime

putFile
  :: ( FilePut m v
     , MetadataPut m
     , MediaTypeDiscover m v
     , Time m
     , MonadLogger m
     )
  => v
  -> Maybe MediaType
  -> m URI
putFile v maybeMT = do

  -- Get the current time
  t <- getCurrentTime

  -- Use given media type or discover what it is.
  mt <- maybe (getMediaType v) pure maybeMT

  -- Add file to blob store.
  blobUrl <- putFileInStore v

  -- Add the metadata to the linked data store.
  let fileName = "some-file.txt"
  fileUrl <- putFileMetadata blobUrl fileName t mt

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
    <> " and media type " <> mediaTypeToText mt
    <> " at " <> show t
