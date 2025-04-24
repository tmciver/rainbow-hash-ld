{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.View.Home (Home(..)) where

import Protolude

import Lucid

import RainbowHash.View.File (File(..))

data Home = Home [File]

instance ToHtml Home where
  toHtml (Home files) = html_ $ do
    body_ $ do
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
          input_ [type_ "text", name_ "title"]
        br_ []
        input_ [type_ "submit", value_ "Submit"]

      toHtml files

  toHtmlRaw = toHtml
