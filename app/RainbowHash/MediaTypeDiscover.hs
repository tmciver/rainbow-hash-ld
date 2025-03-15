{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.MediaTypeDiscover
  ( discoverMediaTypeBS
  , discoverMediaTypeFP
  ) where

import Protolude

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Magic
import Network.HTTP.Media (MediaType, parseAccept, (//))
import System.IO (hFlush)
import System.IO.Temp (withSystemTempFile)

defaultMediaType :: MediaType
defaultMediaType = "application" // "octet-stream"

discoverMediaTypeBS :: ByteString -> IO MediaType
discoverMediaTypeBS bs =
  withSystemTempFile "rainbowhash-" $ \fp h -> do
    BS.hPut h bs
    hFlush h
    magic <- magicOpen [MagicMime]
    magicLoadDefault magic
    mime <- magicFile magic fp
    mime & T.pack
         & T.encodeUtf8
         & parseAccept
         & fromMaybe defaultMediaType
         & pure

discoverMediaTypeFP :: FilePath -> IO MediaType
discoverMediaTypeFP fp = BS.readFile fp >>= discoverMediaTypeBS
