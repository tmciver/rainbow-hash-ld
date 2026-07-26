{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.Upload (UploadWizard (..)) where

import Protolude hiding (for_)

import Lucid

data UploadWizard = UploadWizard

instance ToHtml UploadWizard where
  toHtml UploadWizard = html_ $ do
    head_ $ do
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      title_ "Upload Files"
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css"]
      style_ wizardCss
    body_ [class_ "p-3"] $ do
      -- Page 1: choose single file or directory
      div_ [id_ "page-1"] $ do
        h4_ [class_ "mb-4"] "Upload Files"
        p_ [class_ "text-muted"] "Choose how you'd like to upload:"
        div_ [class_ "d-flex"] $ do
          button_ [ id_ "single-file-btn"
                  , type_ "button"
                  , classes_ ["btn", "btn-outline-primary", "mr-3", "upload-choice-btn"]
                  ] $ do
            div_ [class_ "upload-choice-icon"] "📄"
            div_ "Single File"
          button_ [ id_ "dir-file-btn"
                  , type_ "button"
                  , classes_ ["btn", "btn-outline-primary", "upload-choice-btn"]
                  ] $ do
            div_ [class_ "upload-choice-icon"] "📁"
            div_ "Directory of Files"
        input_ [type_ "file", id_ "single-file-input", style_ "display:none"]
        input_ [type_ "file", id_ "dir-file-input", style_ "display:none", term "webkitdirectory" "", multiple_ ""]

      -- Page 2: review and upload one file at a time
      div_ [id_ "page-2", style_ "display:none"] $ do
        div_ [classes_ ["d-flex", "align-items-center", "mb-3"]] $ do
          h4_ [class_ "mb-0"] "Review & Upload"
          span_ [id_ "file-progress", classes_ ["ml-auto", "text-muted", "small"]] ""
        div_ [class_ "row"] $ do
          div_ [class_ "col-md-7"] $
            div_ [id_ "file-preview", class_ "file-preview-box"] ""
          div_ [class_ "col-md-5"] $ do
            p_ [id_ "current-filename", classes_ ["font-weight-bold", "text-truncate", "mb-3"]] ""
            div_ [class_ "form-group"] $ do
              label_ [for_ "wizard-title"] "Title"
              input_ [type_ "text", id_ "wizard-title", class_ "form-control", placeholder_ "Optional title"]
            div_ [class_ "form-group"] $ do
              label_ [for_ "wizard-description"] "Description"
              textarea_ [id_ "wizard-description", class_ "form-control", rows_ "2", placeholder_ "Optional description"] ""
            div_ [class_ "form-group"] $ do
              label_ "Subjects"
              div_ [id_ "subject-pills", class_ "mb-1"] ""
              div_ [class_ "input-group"] $ do
                div_ [class_ "position-relative", style_ "flex:1"] $ do
                  input_ [ type_ "text"
                         , id_ "subject-concept-input"
                         , class_ "form-control form-control-sm"
                         , placeholder_ "Search concepts..."
                         , autocomplete_ "off"
                         ]
                  div_ [ id_ "concept-suggestions"
                       , class_ "list-group"
                       , style_ "position:absolute;z-index:1000;width:100%;display:none"
                       ] ""
                div_ [class_ "input-group-append"] $
                  button_ [type_ "button", id_ "add-subject-btn", classes_ ["btn", "btn-sm", "btn-secondary"]] "Add"
              div_ [id_ "subject-hidden-inputs"] ""
            div_ [id_ "upload-status", class_ "mb-2"] ""
            div_ [class_ "d-flex"] $ do
              button_ [id_ "skip-btn", type_ "button", classes_ ["btn", "btn-secondary", "mr-2"]] "Skip"
              button_ [id_ "upload-btn", type_ "button", class_ "btn btn-primary"] "Upload"

      -- Page 3: summary
      div_ [id_ "page-3", style_ "display:none"] $ do
        h4_ [class_ "mb-3"] "Done"
        div_ [id_ "uploaded-section"] $ do
          h6_ "Uploaded"
          ul_ [id_ "uploaded-list"] ""
        div_ [id_ "skipped-section"] $ do
          h6_ "Skipped"
          ul_ [id_ "skipped-list"] ""
        button_ [id_ "done-btn", type_ "button", class_ "btn btn-primary mt-3"] "Done"

    script_ [src_ "/static/subject-pills.js"] ("" :: Text)
    script_ [src_ "/static/upload-wizard.js"] ("" :: Text)
  toHtmlRaw = toHtml

wizardCss :: Text
wizardCss =
  ".upload-choice-btn { width: 140px; height: 100px; display: flex; flex-direction: column;\
  \ align-items: center; justify-content: center; }\
  \.upload-choice-icon { font-size: 2rem; margin-bottom: 0.25rem; }\
  \.file-preview-box { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;\
  \ min-height: 300px; display: flex; align-items: center; justify-content: center;\
  \ overflow: hidden; }\
  \.file-preview-box img { max-width: 100%; max-height: 380px; object-fit: contain; }\
  \.file-preview-box iframe { width: 100%; height: 380px; border: 0; }\
  \.file-preview-box pre { max-height: 380px; overflow: auto; padding: 0.5rem;\
  \ font-size: 0.8rem; margin: 0; white-space: pre-wrap; }"
