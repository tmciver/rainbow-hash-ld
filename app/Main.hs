{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude

import           Network.Wai.Handler.Warp (run)

import           RainbowHash.Server       (app)

main :: IO ()
main = run 8081 app
