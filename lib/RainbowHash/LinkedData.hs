{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.LinkedData
  ( putFile
  , FilePut(..)
  , MetadataPut(..)
  , MediaTypeDiscover(..)
  , Time(..)
  , MediaTypeName
  , CharSet
  , MediaType(..)
  ) where

import Protolude

import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Time.Clock (UTCTime)
import Network.URL (URL, exportURL)

import RainbowHash.MediaType

class Monad m => FilePut m v where
  putFileInStore :: v -> m URL

class Monad m => MetadataPut m where
  putFileMetadata :: URL -> UTCTime -> MediaType -> m URL

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
  -> m URL
putFile v maybeMT = do

  -- Get the current time
  t <- getCurrentTime

  -- Use given media type or discover what it is.
  mt <- case maybeMT of
    Just mt' -> pure mt'
    Nothing -> getMediaType v

  -- Add file to blob store.
  blobUrl <- putFileInStore v

  -- Add the metadata to the linked data store.
  fileUrl <- putFileMetadata blobUrl t mt

  logPutFile fileUrl blobUrl t mt

  pure fileUrl

logPutFile
  :: MonadLogger m
  => URL
  -> URL
  -> UTCTime
  -> MediaType
  -> m ()
logPutFile fileUrl blobUrl t mt =
  logInfoN
    $  "Created file object " <> toS (exportURL fileUrl)
    <> " with blob URL " <> toS (exportURL blobUrl)
    <> " and media type " <> mediaTypeToText mt
    <> " at " <> show t
