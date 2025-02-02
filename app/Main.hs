{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Data.Maybe (fromJust)
import Network.URL (importURL, URL)

import RainbowHash.LinkedData (putFile, MediaType(..))
import RainbowHash.App (runApp, appErrorToString, Env(..))

main :: IO ()
main = do
  let file :: FilePath
      file = "/some/file.txt"
      -- mediaType = Nothing
      mediaType = Just $ MediaType "application/octet-stream" ""
      blobStorageUrl :: URL
      blobStorageUrl = "http://localhost:3030/blobs" & importURL & fromJust
      sparqlEndpoint :: URL
      sparqlEndpoint = "http://localhost:3031/sparql" & importURL & fromJust
      env :: Env
      env = Env blobStorageUrl sparqlEndpoint
  --putStrLn $ "Putting file " <> file <> " in store."

  either' <- runApp (putFile file mediaType) env
  case either' of
    Left e -> putStrLn $ appErrorToString e
    Right _ -> pure ()
