{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import RainbowHash.LinkedData (putFile, MediaType(..))
import RainbowHash.App (runApp)

main :: IO ()
main = do
  let file :: FilePath
      file = "/some/file.txt"
      -- mediaType = Nothing
      mediaType = Just $ MediaType "application/octet-stream" ""
  putStrLn $ "Putting file " <> file <> " in store."

  void $ runApp $ putFile file mediaType
