{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import RainbowHash.LinkedData (putFile)
import RainbowHash.App (runApp)

main :: IO ()
main = do
  let file :: FilePath
      file = "/some/file.txt"
      mediaType = Nothing
  putStrLn $ "Putting file " <> file <> " in store."

  void $ runApp $ putFile file mediaType
