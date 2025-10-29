module RainbowHash.EmailAddress
  ( EmailAddress(..)
  , getEmailAddress
  ) where

import Data.Text (Text)

newtype EmailAddress = EmailAddress { getEmailAddress :: Text }
  deriving (Show, Eq, Ord)
