module RainbowHash.LinkedData where

import Protolude

import Network.URL (URL)

type MediaTypeName = Text
type CharSet = Text
data MediaType = MediaType
  { mediaTypeName :: MediaTypeName
  , mediaTypeCharSet :: CharSet
  } deriving (Eq, Ord, Show, Generic)

class FilePut m v where
  putFileInStore :: v -> m URL

class MetadataPut m where
  putFileMetadata :: URL -> MediaType -> m URL

class Monad m => MediaTypeDiscover m v where
  getMediaType :: v -> m MediaType

putFile
  :: ( FilePut m v
     , MetadataPut m
     , MediaTypeDiscover m v
     )
  => v
  -> Maybe MediaType
  -> m URL
putFile v maybeMT = do

  -- Use given media type or discover what it is.
  mt <- case maybeMT of
    Just mt' -> pure mt'
    Nothing -> getMediaType v

  -- Add file to blob store.
  blobUrl <- putFileInStore v

  -- Add the metadata to the linked data store.
  putFileMetadata blobUrl mt
