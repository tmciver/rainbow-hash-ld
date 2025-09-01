{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RainbowHash.View.Home (Home(..)) where

import           Protolude             hiding (for_)

import           Lucid

import           RainbowHash.View.File (File (..))
import RainbowHash.User (User(..))

data Home = Home User [File]

instance ToHtml Home where
  toHtml (Home (User _ mName)files) = html_ $ do
    head_ $ do
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css"]
      link_ [rel_ "stylesheet", href_ "static/style.css"]
    body_ $ do
      div_ [class_ "body-wrapper"] $ do
        h1_ "Caldron"
        let name' = fromMaybe "there" mName
        p_ (toHtml $ "Hello, " <> name' <> "!")
        form_
          [ method_ "POST"
          , action_ "/files"
          , enctype_ "multipart/form-data"
          ] $ do
          div_ [class_ "form-group"] $ do
            label_ [for_ "file-input"] "Upload a File"
            input_ [ type_ "file"
                   , name_ "file"
                   , id_ "file-input"
                   , class_ "form-control-file"
                   ]

          div_ [class_ "form-group"] $ do
            label_ [for_ "title-input"] "Title"
            input_ [ type_ "text"
                   , name_ "title"
                   , placeholder_ "Enter a title for the file"
                   , id_ "title-input"
                   , class_ "form-control"
                   ]

          div_ [class_ "form-group"] $ do
            label_ [for_ "desc-input"] "Description"
            textarea_
              [ name_ "description"
              , placeholder_ "Enter a description of the file"
              , id_ "desc-input"
              , class_ "form-control"
              ]
              (toHtml ("" :: Text))

          div_ [class_ "form-check"] $ do
            input_ [ type_ "checkbox"
                   , name_ "create-new-node"
                   , id_ "new-node-checkbox"
                   , class_ "form-check-input"
                   ]
            label_ [ for_ "new-node-checkbox"
                   , class_ "form-check-label"]
              "Create a new node even if one already exists for this content"

          button_ [type_ "submit", classes_ ["btn", "btn-primary"]] (toHtml ("Submit" :: Text))

        toHtml files

  toHtmlRaw = toHtml
