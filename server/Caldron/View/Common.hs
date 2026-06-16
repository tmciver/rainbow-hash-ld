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
            button_ [type_ "submit", class_ "btn btn-outline-light search-btn"] $
            toHtmlRaw ("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" fill=\"currentColor\" viewBox=\"0 0 16 16\"><path d=\"M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.099zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0\"/></svg>" :: Text)
      div_ [class_ "user-info"] $
        case mUserName of
          Just name -> span_ [class_ "navbar-text text-light"] (toHtml name)
          Nothing   -> pure ()

pageFooter :: Monad m => HtmlT m ()
pageFooter =
  footer_ [classes_ ["bg-dark", "text-light", "px-3", "py-2"]] $
    a_ [class_ "text-light", href_ "/static/file-ontology.html"] "File Ontology"
