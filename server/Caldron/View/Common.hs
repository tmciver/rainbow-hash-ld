{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.Common (navbar, pageFooter) where

import Protolude

import Lucid

navbar :: Monad m => Maybe Text -> HtmlT m ()
navbar mUserName =
  nav_ [classes_ ["navbar", "navbar-dark", "bg-dark", "px-3"]] $ do
    a_ [class_ "navbar-brand mr-3", href_ "/"] "Caldron"
    div_ [classes_ ["ml-auto", "d-flex", "align-items-center"]] $ do
      form_ [class_ "search-form mr-3", method_ "get", action_ "/search"] $
        div_ [class_ "input-group"] $ do
          input_ [ type_ "search"
                 , name_ "q"
                 , class_ "form-control search-input"
                 , placeholder_ "Search files..."
                 ]
          div_ [class_ "input-group-append"] $
            button_ [type_ "submit", class_ "btn btn-outline-light search-btn"] "⌕"
      div_ [class_ "user-info"] $
        case mUserName of
          Just name -> span_ [class_ "navbar-text text-light"] (toHtml name)
          Nothing   -> pure ()

pageFooter :: Monad m => HtmlT m ()
pageFooter =
  footer_ [classes_ ["bg-dark", "text-light", "px-3", "py-2"]] $
    a_ [class_ "text-light", href_ "/static/file-ontology.html"] "File Ontology"
