{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.Common (navbar, pageFooter) where

import Protolude

import Lucid

navbar :: Monad m => Maybe Text -> HtmlT m ()
navbar mUserName =
  nav_ [classes_ ["navbar", "navbar-dark", "bg-dark", "px-3"]] $ do
    a_ [class_ "navbar-brand", href_ "/"] "Caldron"
    case mUserName of
      Just name -> span_ [classes_ ["ml-auto", "navbar-text", "text-light"]] (toHtml name)
      Nothing   -> pure ()

pageFooter :: Monad m => HtmlT m ()
pageFooter =
  footer_ [classes_ ["bg-dark", "text-light", "px-3", "py-2"]] $
    a_ [class_ "text-light", href_ "/static/file-ontology.html"] "File Ontology"
