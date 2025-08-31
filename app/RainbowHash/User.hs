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
  , name :: Maybe Text
  }
