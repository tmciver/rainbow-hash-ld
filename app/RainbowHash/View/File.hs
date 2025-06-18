{-# LANGUAGE FlexibleInstances #-}

module RainbowHash.View.File (File(..)) where

import Protolude

import qualified Data.CaseInsensitive as CI
import Data.Text.Encoding as T
import Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Lucid
import Lucid.Base (makeAttribute)
import Network.HTTP.Media (MediaType, mainType, subType)
import Text.URI (render)

import qualified RainbowHash.File as RH

newtype File = File RH.File

instance ToHtml [File] where
  toHtml [] = pure ()
  toHtml files = do
    h2_ "Recent Files"
    table_ [ makeAttribute "border" "1"
           , classes_ ["table", "table-bordered", "table-hover"]
           ] $ do
      thead_ [class_ "thead-dark"] $ do
        tr_ $ do
          th_ "File name"
          th_ "Title"
          th_ "Description"
          th_ "Media Type"
          th_ "Created"
          th_ "Last Modified"
          th_ "Content"
      tbody_ (foldMap toHtml files)

  toHtmlRaw = toHtml

instance ToHtml File where
  toHtml (File f) = do
    tr_ $ do
      td_ (toHtml . fromMaybe "" . RH.fileName $ f)
      td_ (toHtml . fromMaybe "" . RH.fileTitle $ f)
      td_ (toHtml . fromMaybe "" . RH.fileDescription $ f)
      td_ (toHtml . showMediaType . RH.fileMediaType $ f)
      td_ (toHtml . showUTCTime . RH.fileCreatedAt $ f)
      td_ (toHtml . showUTCTime . RH.fileUpdatedAt $ f)
      td_ (a_ [href_ (render . RH.fileContent $ f)] (toHtml ("Link" :: Text)))

    where showMediaType :: MediaType -> Text
          showMediaType mt = T.decodeUtf8 . CI.original $ mainType mt <> "/" <> subType mt

          showUTCTime :: UTCTime -> Text
          showUTCTime = T.pack . formatTime defaultTimeLocale "%B %e, %Y %l:%M:%S%p %Z"

  toHtmlRaw = toHtml
