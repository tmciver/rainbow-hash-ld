{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.MediaTypeDiscover
  ( discoverMediaTypeBS
  , discoverMediaTypeFP
  ) where

import Protolude

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Magic
import System.IO (hFlush)
import System.IO.Temp (withSystemTempFile)

import RainbowHash.MediaType

defaultMediaType :: MediaType
defaultMediaType = MediaType "application/octet-stream" "binary"

discoverMediaTypeBS :: ByteString -> IO MediaType
discoverMediaTypeBS bs =
  withSystemTempFile "rainbowhash-" $ \fp h -> do
    BS.hPut h bs
    hFlush h
    magic <- magicOpen [MagicMime]
    magicLoadDefault magic
    mime <- magicFile magic fp
    let mt = mime & T.pack & parseMediaType & fromMaybe defaultMediaType
    pure mt

discoverMediaTypeFP :: FilePath -> IO MediaType
discoverMediaTypeFP fp = BS.readFile fp >>= discoverMediaTypeBS
