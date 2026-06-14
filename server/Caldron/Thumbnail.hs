{-# LANGUAGE NoImplicitPrelude #-}

module Caldron.Thumbnail
  ( generateThumbnailBytes
  ) where

import Protolude

import qualified Data.ByteString    as BS
import           System.IO          (hClose)
import           System.IO.Temp     (withSystemTempFile)
import           System.Process     (readProcessWithExitCode)

-- | Generate a JPEG thumbnail for the file at the given path using ImageMagick.
-- The "[0]" suffix selects the first page/frame for multi-page formats (PDF, etc.).
-- Returns Nothing if ImageMagick cannot handle the file type.
generateThumbnailBytes :: FilePath -> IO (Maybe ByteString)
generateThumbnailBytes inputPath =
  withSystemTempFile "caldron-thumb-.jpg" $ \thumbPath h -> do
    hClose h  -- release so ImageMagick can write to it
    (exitCode, _, _) <- readProcessWithExitCode "convert"
      [ inputPath <> "[0]"
      , "-thumbnail", "300x300>"
      , "-background", "white"
      , "-alpha", "remove"
      , "-quality", "85"
      , thumbPath
      ]
      ""
    case exitCode of
      ExitSuccess   -> Just <$> BS.readFile thumbPath
      ExitFailure _ -> pure Nothing
