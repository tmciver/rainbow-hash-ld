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
        div_ [classes_ ["d-flex", "align-items-center", "mb-4"]] $ do
          h2_ [class_ "mb-0"] "Files"
          a_ [ href_ "/upload/wizard"
             , classes_ ["btn", "btn-primary", "btn-sm", "ml-auto"]
             ] "Upload"

        when (not $ null files) $
          h5_ [classes_ ["mt-2", "mb-3", "text-muted"]] "Recent Files"
        toHtml files

      pageFooter
