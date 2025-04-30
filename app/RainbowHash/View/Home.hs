{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.View.Home (Home(..)) where

import Protolude

import Lucid

import RainbowHash.View.File (File(..))

data Home = Home [File]

instance ToHtml Home where
  toHtml (Home files) = html_ $ do
    head_ $ do
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css"]
    body_ $ do
      div_ [class_ "body-wrapper"] $ do
        form_
          [ method_ "POST"
          , action_ "/files"
          , enctype_ "multipart/form-data"
          ] $ do
          div_ $ do
            label_ "Upload a File"
            br_ []
            input_ [type_ "file", name_ "file"]

            br_ []
            label_ "Title"
            br_ []
            input_ [type_ "text", name_ "title", placeholder_ "Enter a title for the file"]

            br_ []
            label_ "Description"
            br_ []
            textarea_ [name_ "description", placeholder_ "Enter a description of the file"] (toHtml ("" :: Text))

          br_ []
          input_ [type_ "submit", value_ "Submit"]

        toHtml files

  toHtmlRaw = toHtml
