{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RainbowHash.EmailAddress
  ( EmailAddress(..)
  ) where

import Protolude

import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

newtype EmailAddress = EmailAddress { getEmailAddress :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)
