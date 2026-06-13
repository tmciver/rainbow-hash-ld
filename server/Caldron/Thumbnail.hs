{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Thumbnail
  ( getThumbnail
  ) where

import Protolude

import qualified Data.ByteString      as BS
import           System.Directory     (doesFileExist, createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.Process       (readProcessWithExitCode)
import qualified Data.Text            as T

-- | Return a JPEG thumbnail for the file at the given content URL, generating
-- and caching it on disk if it does not already exist.  Returns Nothing if
-- ImageMagick cannot handle the file type.
getThumbnail
  :: FilePath  -- ^ Directory in which to cache thumbnails
  -> Text      -- ^ File ID used as the cache key (filename without extension)
  -> Text      -- ^ Content URL that ImageMagick will read from
  -> IO (Maybe ByteString)
getThumbnail cacheDir fileId contentUrl = do
  createDirectoryIfMissing True cacheDir
  let thumbPath = cacheDir </> T.unpack fileId <> ".jpg"
  exists <- doesFileExist thumbPath
  if exists
    then Just <$> BS.readFile thumbPath
    else generate thumbPath

  where
    generate thumbPath = do
      -- Append "[0]" to select the first page/frame for multi-page formats
      -- (PDFs, animated GIFs, multi-frame TIFFs, etc.)
      let input = T.unpack contentUrl <> "[0]"
      (exitCode, _, _) <- readProcessWithExitCode "convert"
        [ input
        , "-thumbnail", "300x300>"  -- shrink to fit; never enlarge
        , "-background", "white"
        , "-alpha", "remove"        -- flatten transparency onto white
        , "-quality", "85"
        , thumbPath
        ]
        ""
      case exitCode of
        ExitSuccess   -> Just <$> BS.readFile thumbPath
        ExitFailure _ -> pure Nothing
