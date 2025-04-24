{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.View.File (File(..)) where

import Protolude

import Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Lucid
import Lucid.Base (makeAttribute)
import Network.HTTP.Media (renderHeader)
import Text.URI (render)

import qualified RainbowHash.File as RH

newtype File = File RH.File

instance ToHtml [File] where
  toHtml [] = pure ()
  toHtml files = do
    h2_ "Recent Files"
    table_ [makeAttribute "border" "1"] $ do
      tr_ $ do
        th_ "File name"
        th_ "Title"
        th_ "Media Type"
        th_ "Created"
        th_ "Last Modified"
        th_ "Content"
      foldMap toHtml files

  toHtmlRaw = toHtml

instance ToHtml File where
  toHtml (File f) = do
    tr_ $ do
      td_ (toHtml . fromMaybe "" . RH.fileName $ f)
      td_ (toHtml . fromMaybe "" . RH.fileTitle $ f)
      td_ (toHtml . T.decodeUtf8 . renderHeader . RH.fileMediaType $ f)
      td_ (toHtml . (show :: UTCTime -> Text) . RH.fileCreatedAt $ f)
      td_ (toHtml . (show :: UTCTime -> Text) . RH.fileUpdatedAt $ f)
      td_ (a_ [href_ (render . RH.fileContent $ f)] (toHtml ("Link" :: Text)))

  toHtmlRaw = toHtml
