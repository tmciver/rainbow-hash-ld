{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.Home (Home(..)) where

import           Protolude             hiding (for_)

import           Lucid

import Caldron.View.Common (navbar, pageFooter)
import Caldron.View.File (File (..))
import Caldron.User (User, userName)

data Home = Home User [File]

instance ToHtml Home where
  toHtml (Home user files) = html_ $ do
    head_ $ do
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      title_ "Caldron"
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css"]
      link_ [rel_ "stylesheet", href_ "static/style.css"]
    body_ $ do
      navbar (userName user)
      div_ [classes_ ["container", "mt-4"]] $ do
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

          div_ [class_ "form-group"] $ do
            label_ "Subject (SKOS Concepts)"
            div_ [id_ "subject-pills", classes_ ["mb-2"]] (pure ())
            div_ [class_ "input-group"] $ do
              div_ [class_ "position-relative", style_ "flex: 1"] $ do
                input_ [ type_ "text"
                       , id_ "subject-concept-input"
                       , class_ "form-control"
                       , placeholder_ "Search for a concept..."
                       , autocomplete_ "off"
                       ]
                div_ [ id_ "concept-suggestions"
                     , class_ "list-group"
                     , style_ "position: absolute; z-index: 1000; width: 100%; display: none"
                     ] (pure ())
              div_ [class_ "input-group-append"] $
                button_ [ type_ "button"
                        , id_ "add-subject-btn"
                        , class_ "btn btn-secondary"
                        ] "Add"
            div_ [id_ "subject-hidden-inputs"] (pure ())

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

        script_ [src_ "/static/subject-pills.js"] ("" :: Text)

      pageFooter

  toHtmlRaw = toHtml
