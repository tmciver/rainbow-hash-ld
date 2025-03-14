{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.Config
  ( getConfig
  , Config(..)
  ) where

import Protolude

-- use of fromJust will go away once we're no longer hard-coding data here.
import Data.Maybe (fromJust)
import Text.URI (mkURI, URI)

data Config = Config
  { blobStoreUrl :: URI
  , sparqlEndpoint :: URI
  }

getConfig :: IO Config
getConfig = do
  let blobStorageUrl :: URI
      blobStorageUrl = "http://localhost:3001/blobs" & mkURI & fromJust
      sparqlEndpoint :: URI
      sparqlEndpoint = "http://localhost:3030/ds" & mkURI & fromJust
  pure $ Config blobStorageUrl sparqlEndpoint
