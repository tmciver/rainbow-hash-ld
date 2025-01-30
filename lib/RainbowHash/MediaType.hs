{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.MediaType
  ( MediaType(..)
  , MediaTypeName
  , CharSet
  , mediaTypeToText
  , parseMediaType
  ) where

import Protolude

import qualified Data.Text as T

type MediaTypeName = Text
type CharSet = Text
data MediaType = MediaType
  { mediaTypeName :: MediaTypeName
  , mediaTypeCharSet :: CharSet
  } deriving (Eq, Ord, Show, Generic)

mediaTypeToText :: MediaType -> Text
mediaTypeToText (MediaType name "") = name
mediaTypeToText (MediaType name charSet) = name <> "; charset=" <> charSet

parseMediaType :: Text -> Maybe MediaType
parseMediaType t = case T.splitOn ";" t of
  [ct, charSet] -> if " charset=" `T.isPrefixOf` charSet
    then Just $ MediaType (T.strip ct) (T.drop 9 charSet)
    else Nothing
  _ -> Nothing
