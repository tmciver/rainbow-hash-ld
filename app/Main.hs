{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Data.Maybe (fromJust)
import Text.URI (mkURI, URI)

import RainbowHash.LinkedData (putFile)
import RainbowHash.MediaType (MediaType(..))
import RainbowHash.App (runApp, appErrorToString, Env(..))

main :: IO ()
main = do
  let file :: FilePath
      file = "/home/tim/workspace/haskell/rainbow-hash-ld/default.nix"
      -- mediaType = Nothing
      --mediaType = Just $ MediaType "application/octet-stream" ""
      mediaType = Nothing
      blobStorageUrl :: URI
      blobStorageUrl = "http://localhost:3030/blobs" & mkURI & fromJust
      sparqlEndpoint :: URI
      sparqlEndpoint = "http://localhost:3031/sparql" & mkURI & fromJust
      env :: Env
      env = Env blobStorageUrl sparqlEndpoint
  --putStrLn $ "Putting file " <> file <> " in store."

  either' <- runApp (putFile file mediaType) env
  case either' of
    Left e -> putStrLn $ appErrorToString e
    Right _ -> pure ()
