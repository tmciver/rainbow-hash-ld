{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.Search (SearchResults(..)) where

import Protolude hiding (for_)

import Lucid

import Caldron.User         (User, userName)
import Caldron.View.Common  (navbar, pageFooter)
import Caldron.View.File    (File (..))

data SearchResults = SearchResults User Text [File]

instance ToHtml SearchResults where
  toHtml (SearchResults user q files) = html_ $ do
    head_ $ do
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      title_ "Search — Caldron"
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css"]
      link_ [rel_ "stylesheet", href_ "/static/style.css"]
    body_ $ do
      navbar (userName user)
      div_ [classes_ ["container", "mt-4"]] $ do
        h2_ [class_ "mb-3"] $ toHtml ("Results for: " <> q)
        toHtml files
      pageFooter

  toHtmlRaw = toHtml
