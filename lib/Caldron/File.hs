module Caldron.File (File(..), FileRevision(..)) where

import           Protolude

import           Data.Time.Clock    (UTCTime)
import           Network.HTTP.Media (MediaType)
import           Text.URI           (URI)

data FileRevision = FileRevision
  { revisionUri     :: URI
  , revisionCreated :: UTCTime
  , revisionSize    :: Integer
  }

data File = File
  { fileUri         :: URI
  , fileName        :: Maybe Text
  , fileSize        :: Integer
  , fileTitle       :: Maybe Text
  , fileDescription :: Maybe Text
  , fileMediaType   :: MediaType
  , fileCreatedAt   :: UTCTime
  , fileUpdatedAt   :: UTCTime -- will equal fileCreatedAt if it has not been updated after creation.
  , fileContent     :: URI
  , fileSubjects    :: [URI]
  , fileThumbnail   :: Maybe URI
  }
