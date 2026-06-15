{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.Common (navbar) where

import Protolude

import Lucid

navbar :: Monad m => HtmlT m ()
navbar =
  nav_ [classes_ ["navbar", "navbar-dark", "bg-dark", "px-3"]] $ do
    a_ [class_ "navbar-brand", href_ "/"] "Caldron"
    a_ [class_ "nav-link text-light", href_ "/static/file-ontology.html"] "Ontology"
