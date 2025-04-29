module RainbowHash.File (File(..)) where

import Protolude

import Data.Time.Clock (UTCTime)
import Network.HTTP.Media (MediaType)
import Text.URI (URI)

data File = File
  { fileUri :: URI
  , fileName :: Maybe Text
  , fileTitle :: Maybe Text
  , fileDescription :: Maybe Text
  , fileMediaType :: MediaType
  , fileCreatedAt :: UTCTime
  , fileUpdatedAt :: UTCTime -- will equal fileCreatedAt if it has not been updated after creation.
  , fileContent :: URI
  }
