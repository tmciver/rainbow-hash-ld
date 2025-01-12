{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Data.Maybe (fromJust)
import Network.URL (importURL, URL)

import RainbowHash.LinkedData (putFile, MediaType(..))
import RainbowHash.App (runApp, Env(..))

main :: IO ()
main = do
  let file :: FilePath
      file = "/some/file.txt"
      -- mediaType = Nothing
      mediaType = Just $ MediaType "application/octet-stream" ""
      blobStorageUrl :: URL
      blobStorageUrl = "http://localhost:3030/blobs" & importURL & fromJust
      env :: Env
      env = Env blobStorageUrl
  putStrLn $ "Putting file " <> file <> " in store."

  void $ runApp (putFile file mediaType) env
