{-# LANGUAGE MultiParamTypeClasses #-}

module RainbowHash.LinkedData
  ( putFile
  , FilePut(..)
  , FileGet(..)
  , MetadataPut(..)
  , MediaTypeDiscover(..)
  , FileNameGet(..)
  , Time(..)
  , FileNodeCreateOption(..)
  ) where

import Protolude

import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Media (MediaType, renderHeader)
import Text.URI (URI, render)

import RainbowHash.File (File)

data FileNodeCreateOption
  = AlwaysCreate
  -- ^Creates a new FileDataObject (NEPOMUK) even if one already exists for this
  -- content.
  | CreateIfNotExists
  -- ^Only creates a new FileDataObject if one does not exist for this
  -- content. Returns the URI for the existing FileDataObject otherwise.
  deriving (Show)

class Monad m => FilePut m v where
  putFileInStore :: v -> m URI

class Monad m => FileGet m where
  getFile :: URI -> m (Maybe File)
  getRecentFiles :: m [File]
  getFileForContent :: URI -> m (Maybe URI)

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
  :: ( FileGet m
     , FilePut m v
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
  -> FileNodeCreateOption
  -> m URI
putFile v createdByUri maybeFileName maybeTitle maybeDesc maybeMT fileNodeCreateOption = do

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

  -- What is done next depends on the value of fileNodeCreateOption
  case fileNodeCreateOption of
    AlwaysCreate ->
      putFileMetadata blobUrl createdByUri maybeFileName' maybeTitle maybeDesc t mt
      >>= logPutFile blobUrl t mt
    CreateIfNotExists -> do
      maybeFileUrl <- getFileForContent blobUrl
      case maybeFileUrl of
        Just fileUrl' -> do
          logInfoN "FileDataObject already exists for this content; returning existing URL."
          -- TODO: should the title and description be updated to what's given in this request?
          pure fileUrl'
        Nothing ->
          putFileMetadata blobUrl createdByUri maybeFileName' maybeTitle maybeDesc t mt
          >>= logPutFile blobUrl t mt

-- Logs the data used to create a file object.
-- Takes the file URI argument last and returns it to facilitate monadic
-- sequencing.
logPutFile
  :: MonadLogger m
  => URI
  -> UTCTime
  -> MediaType
  -> URI
  -> m URI
logPutFile blobUrl t mt fileUrl = do
  logInfoN
    $  "Created file object " <> toS (render fileUrl)
    <> " with blob URI " <> toS (render blobUrl)
    <> " and media type " <> T.decodeUtf8 (renderHeader mt)
    <> " at " <> show t
  pure fileUrl
