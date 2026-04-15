{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Options.Applicative (execParser)

import Caldron.Command (runCommand, options)
import Caldron.CLI.Config (getStoredConfig)

main :: IO ()
main = do
  storedConfig <- getStoredConfig
  cmd <- execParser options
  runCommand storedConfig cmd
