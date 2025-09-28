{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.LinkedData
  ( putFile
  , updateFileContent
  , FilePut(..)
  , FileGet(..)
  , MetadataPut(..)
  , MediaTypeDiscover(..)
  , FileNameGet(..)
  , FileSizeGet(..)
  , Time(..)
  , FileNodeCreateOption(..)
  ) where

import           Protolude

import           Control.Monad.Logger (MonadLogger (..), logInfoN)
import           Data.Text.Encoding   as T
import           Data.Time.Clock      (UTCTime)
import           Network.HTTP.Media   (MediaType, renderHeader)
import           Text.URI             (URI, render)

import           RainbowHash.File     (File, fileMediaType, fileContent)

data FileNodeCreateOption
  = AlwaysCreate
  -- ^Creates a new File object even if one already exists for this
  -- content.
  | CreateIfNotExists
  -- ^Only creates a new File object if one does not exist for this
  -- content. Returns the URI for the existing File object otherwise.
  deriving (Show)

data FileError
  = FileNotFound URI
  | MediaTypeMismatch

class Monad m => FilePut m v where
  putFileInStore :: v -> m URI

class Monad m => FileGet m where
  getFile :: URI -> m (Maybe File)
  getRecentFiles :: m [File]
  getFileForContent :: URI -> m (Maybe URI)

class Monad m => MetadataPut m where
  putFileMetadata
    :: Text -- ^Hostname from HTTP request
    -> URI -- ^URI of file data in blob storage
    -> URI -- ^URI of agent creating the file
    -> Maybe Text -- ^file name. May be unavailable if client calls putFile on ByteString.
    -> Integer    -- ^file size
    -> Maybe Text -- ^title
    -> Maybe Text -- ^description
    -> UTCTime -- ^file creation time
    -> MediaType
    -> m URI

  updateFileGraphWithContent
    :: URI -- ^File object URI
    -> URI -- ^URI of file data in blob storage
    -> URI -- ^URI of agent creating the file
    -> Integer    -- ^file size
    -> UTCTime -- ^file creation time
    -> m ()

class Monad m => MediaTypeDiscover m v where
  getMediaType :: v -> m MediaType

class Monad m => FileNameGet m v where
  getFileName :: v -> m (Maybe Text)

class Monad m => FileSizeGet m v where
  getFileSize :: v -> m Integer

class Monad m => Time m where
  getCurrentTime :: m UTCTime

putFile
  :: ( FileGet m
     , FilePut m v
     , MetadataPut m
     , MediaTypeDiscover m v
     , FileNameGet m v
     , FileSizeGet m v
     , Time m
     , MonadLogger m
     )
  => v
  -> Text
  -> URI -- ^URI of agent putting the file
  -> Maybe Text -- ^filename
  -> Maybe Text -- ^title
  -> Maybe Text -- ^description
  -> Maybe MediaType
  -> FileNodeCreateOption
  -> m URI
putFile v host createdByUri maybeFileName maybeTitle maybeDesc maybeMT fileNodeCreateOption = do

  logInfoN $ "Adding file "
    <> fromMaybe "<unnamed>" (maybeFileName <&> \fn -> "\"" <> fn <> "\"")
    <> " with title "
    <> fromMaybe "<notitle>" (maybeTitle <&> \title -> "\"" <> title <> "\"")

  -- Get the current time
  t <- getCurrentTime

  -- Use given media type or discover what it is.
  mt <- maybe (getMediaType v) pure maybeMT

  logInfoN $ "Media type: " <> show mt

  -- Use the given filename or get it from v
  maybeFileName' <- case maybeFileName of
    Just fn -> pure $ Just fn
    Nothing -> getFileName v

  -- Get the file's size
  size <- getFileSize v

  -- Add file to blob store.
  blobUrl <- putFileInStore v

  logInfoN $ "Added file to store at URL " <> render blobUrl

  -- What is done next depends on the value of fileNodeCreateOption
  case fileNodeCreateOption of
    AlwaysCreate -> do
      logInfoN "User requested creation of a new file node (even if one already exists)."
      putFileMetadata host blobUrl createdByUri maybeFileName' size maybeTitle maybeDesc t mt
      >>= logPutFile blobUrl t mt
    CreateIfNotExists -> do
      maybeFileUrl <- getFileForContent blobUrl
      case maybeFileUrl of
        Just fileUrl' -> do
          logInfoN "File object already exists for this content; returning existing URL."
          -- TODO: should the title and description be updated to what's given in this request?
          pure fileUrl'
        Nothing -> do
          logInfoN "No file object exists for this content; creating a new file object."
          putFileMetadata host blobUrl createdByUri maybeFileName' size maybeTitle maybeDesc t mt
          >>= logPutFile blobUrl t mt

updateFileContent
  :: ( FileGet m
     , FilePut m v
     , MetadataPut m
     , MediaTypeDiscover m v
     , FileNameGet m v
     , FileSizeGet m v
     , Time m
     , MonadError FileError m
     , MonadLogger m
     )
  => URI -- ^URI of File object to be updated
  -> v   -- ^File content
  -> URI -- ^URI of agent putting the file
  -> Maybe MediaType
  -> m ()
updateFileContent fileUri v createdByUri maybeMT = do
  logInfoN $ "Updating file " <> render fileUri

  -- Get the file object for the given fle URI
  file <- getFile fileUri >>= maybe (throwError $ FileNotFound fileUri) pure

  -- Get the current time
  t <- getCurrentTime

  -- Use given media type or discover what it is.
  mt <- maybe (getMediaType v) pure maybeMT

  -- media type of new content must be the same as the existing content
  when (mt /= fileMediaType file) $
    throwError MediaTypeMismatch

  logInfoN $ "Media type: " <> show mt

  -- Get the file's size
  size <- getFileSize v

  -- Add file to blob store.
  blobUrl <- putFileInStore v

  -- Check to see if the new content is the same as the existing content.
  -- If so, nothing will be done.
  when (blobUrl /= fileContent file) $ do
    logInfoN $ "Added file to store at URL " <> render blobUrl

    updateFileGraphWithContent fileUri blobUrl createdByUri size t

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
