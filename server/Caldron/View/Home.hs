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
      style_ modalCss
    body_ $ do
      navbar (userName user)
      div_ [classes_ ["container", "mt-4"]] $ do
        div_ [classes_ ["d-flex", "align-items-center", "mb-4"]] $ do
          h2_ [class_ "mb-0"] "Files"
          button_ [ type_ "button"
                  , classes_ ["btn", "btn-primary", "btn-sm", "ml-auto"]
                  , onclick_ "openUploadWizard()"
                  ] "Upload"

        when (not $ null files) $
          h5_ [classes_ ["mt-2", "mb-3", "text-muted"]] "Recent Files"
        toHtml files

      -- Upload wizard modal
      div_ [id_ "upload-modal-overlay", class_ "upload-modal-overlay"] $
        div_ [class_ "upload-modal-box"] $ do
          button_ [ type_ "button"
                  , class_ "upload-modal-close"
                  , onclick_ "closeUploadModal()"
                  ] "\xd7"
          iframe_ [id_ "upload-wizard-frame", class_ "upload-modal-frame", src_ ""] ""

      pageFooter
      script_ modalJs

  toHtmlRaw = toHtml

modalCss :: Text
modalCss =
  ".upload-modal-overlay { display:none; position:fixed; top:0; left:0; width:100%; height:100%;\
  \ background:rgba(0,0,0,0.5); z-index:1050; align-items:center; justify-content:center; }\
  \.upload-modal-overlay.open { display:flex; }\
  \.upload-modal-box { position:relative; background:#fff; border-radius:6px;\
  \ width:880px; max-width:95vw; height:620px; max-height:90vh; display:flex; flex-direction:column; }\
  \.upload-modal-close { position:absolute; top:8px; right:12px; background:none; border:none;\
  \ font-size:1.5rem; line-height:1; cursor:pointer; z-index:1; }\
  \.upload-modal-frame { flex:1; border:none; border-radius:6px; width:100%; height:100%; }"

modalJs :: Text
modalJs =
  "function openUploadWizard() {\
  \  document.getElementById('upload-wizard-frame').src = '/upload/wizard';\
  \  document.getElementById('upload-modal-overlay').classList.add('open');\
  \}\
  \function closeUploadModal() {\
  \  document.getElementById('upload-modal-overlay').classList.remove('open');\
  \  document.getElementById('upload-wizard-frame').src = '';\
  \}\
  \window.addEventListener('message', function(e) {\
  \  if (e.data === 'upload-wizard-done') { closeUploadModal(); location.reload(); }\
  \});"
