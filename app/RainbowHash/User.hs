{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.User
  ( User(..)
  , WebID
  ) where

import           Protolude

import           Text.URI                         (URI)

type WebID = URI

data User = User
  { webId     :: WebID
  , firstName :: Maybe Text
  , lastName  :: Maybe Text
  }
