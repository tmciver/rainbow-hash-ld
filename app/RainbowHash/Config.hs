{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RainbowHash.Config
  ( Config(..)
  ) where

import Protolude

import           Text.URI   (URI)

data Config = Config
  { blobStoreUrl   :: URI
  , sparqlEndpoint :: URI
  -- If present, use the configured host over that provided in the Host header
  , preferredHost :: Maybe Text
  }
