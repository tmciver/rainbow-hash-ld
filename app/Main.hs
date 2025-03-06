{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Data.Maybe (fromJust)
import Text.URI (mkURI, URI)

import RainbowHash.LinkedData (putFile)
import RainbowHash.App (runApp, appErrorToString, Env(..))

main :: IO ()
main = do
  let file :: FilePath
      file = "/home/tim/workspace/haskell/rainbow-hash-ld/default.nix"
      mediaType = Nothing
      fileName = Nothing
      agentUri = "http://timmciver.com/me#" & mkURI & fromJust

  env <- getEnv

  either' <- runApp (putFile file agentUri fileName mediaType) env
  whenLeft either' (putStrLn . appErrorToString)

getEnv :: IO Env
getEnv = do
  let blobStorageUrl :: URI
      blobStorageUrl = "http://localhost:3001/blobs" & mkURI & fromJust
      sparqlEndpoint :: URI
      sparqlEndpoint = "http://localhost:3030/ds" & mkURI & fromJust
      env :: Env
      env = Env blobStorageUrl sparqlEndpoint
  pure env

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left v) f = f v
whenLeft _ _ = pure ()
