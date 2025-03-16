{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.View.Home (Home(..)) where

import Protolude

import Lucid

data Home = Home

instance ToHtml Home where
  toHtml _ = html_ $ do
    body_ $ do
      form_
       [ method_ "POST"
       , action_ "/files"
       , enctype_ "multipart/form-data"
       ] $ do
        div_ $ do
          label_ "Upload a File"
          br_ []
          input_ [type_ "file", name_ "filename"]
        br_ []
        input_ [type_ "submit", value_ "Submit"]

  toHtmlRaw = toHtml
