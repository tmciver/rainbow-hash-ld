{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.Common (navbar, pageFooter) where

import Protolude

import Lucid

navbar :: Monad m => Maybe Text -> HtmlT m ()
navbar mUserName =
  nav_ [classes_ ["navbar", "navbar-dark", "bg-dark", "px-3"]] $ do
    a_ [class_ "navbar-brand mr-3", href_ "/"] "Caldron"
    form_ [class_ "form-inline", method_ "get", action_ "/search"] $ do
      input_ [ type_ "search"
             , name_ "q"
             , class_ "form-control form-control-sm"
             , placeholder_ "Search..."
             ]
      button_ [type_ "submit", classes_ ["btn", "btn-outline-light", "btn-sm", "ml-2"]] "Search"
    case mUserName of
      Just name -> span_ [classes_ ["ml-auto", "navbar-text", "text-light"]] (toHtml name)
      Nothing   -> pure ()

pageFooter :: Monad m => HtmlT m ()
pageFooter =
  footer_ [classes_ ["bg-dark", "text-light", "px-3", "py-2"]] $
    a_ [class_ "text-light", href_ "/static/file-ontology.html"] "File Ontology"
