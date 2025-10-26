{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Options.Applicative (execParser)

import RainbowHash.Command (runCommand, options)
import RainbowHash.CLI.Config (getConfig)

main :: IO ()
main = do
  config <- getConfig
  cmd <- execParser options
  runCommand config cmd
