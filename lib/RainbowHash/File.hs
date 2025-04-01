module RainbowHash.File (File(..)) where

import Protolude

import Data.Time.Clock (UTCTime)
import Network.HTTP.Media (MediaType)
import Text.URI (URI)

data File = File
  { fileUri :: URI
  , fileName :: Maybe Text
  , fileMediaType :: MediaType
  , fileCreatedAt :: UTCTime
  , fileUpdatedAt :: Maybe UTCTime
  , fileContent :: URI
  }
