{-# LANGUAGE FlexibleInstances #-}

module Caldron.View.File (File(..)) where

import           Protolude hiding (for_)

import qualified Data.CaseInsensitive as CI

import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Data.Time.Clock      (UTCTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Lucid
import           Network.HTTP.Media   (MediaType, mainType, subType)
import           Text.URI             (render)

import           Caldron.Concept  (Concept)
import qualified Caldron.Concept  as Concept
import qualified Caldron.File     as RH

data File = File RH.File [Concept] [RH.FileRevision]

-- ---------------------------------------------------------------------------
-- Helpers

showMediaType :: MediaType -> Text
showMediaType mt = T.decodeUtf8 . CI.original $ mainType mt <> "/" <> subType mt

showUTCTime :: UTCTime -> Text
showUTCTime = T.pack . formatTime defaultTimeLocale "%B %e, %Y %l:%M:%S%p %Z"

-- ---------------------------------------------------------------------------
-- Recent-files gallery (used on the home page)

instance ToHtml [File] where
  toHtml [] = pure ()
  toHtml files = do
    h2_ [classes_ ["mt-4", "mb-3"]] "Recent Files"
    div_ [class_ "row"] $
      forM_ files $ \(File f _ _) -> do
        let fileLink = render (RH.fileUri f)
            label    = fromMaybe (fromMaybe "" (RH.fileName f)) (RH.fileTitle f)
        div_ [classes_ ["col-6", "col-md-4", "col-lg-3", "mb-4"]] $
          a_ [href_ fileLink, classes_ ["card", "h-100", "text-decoration-none", "text-dark"]] $ do
            let thumbSrc = case RH.fileThumbnail f of
                  Just _  -> fileLink <> "/thumbnail"
                  Nothing -> "/static/no-preview.jpg"
            img_ [ src_ thumbSrc
                 , class_ "card-img-top"
                 , style_ "height: 140px; object-fit: cover;"
                 , alt_ "File thumbnail"
                 ]
            div_ [classes_ ["card-body", "p-2"]] $
              h6_ [classes_ ["card-title", "mb-0"]] (toHtml label)
            div_ [classes_ ["card-footer", "text-muted", "small", "p-2"]] $ do
              div_ (toHtml $ showMediaType (RH.fileMediaType f))
              div_ (toHtml $ showUTCTime (RH.fileCreatedAt f))

  toHtmlRaw = toHtml

-- ---------------------------------------------------------------------------
-- Single-file view page

instance ToHtml File where
  toHtml (File f concepts revisions) = html_ $ do
    head_ $ do
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      title_ (toHtml (fromMaybe "File" (RH.fileTitle f) <> " — Caldron"))
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css"]
      link_ [rel_ "stylesheet", href_ "/static/style.css"]
    body_ [style_ "overflow: hidden"] $ do
      nav_ [classes_ ["navbar", "navbar-dark", "bg-dark", "px-3"]] $
        a_ [class_ "navbar-brand", href_ "/"] "Caldron"
      div_ [class_ "container-fluid", style_ "height: calc(100vh - 56px)"] $
        div_ [classes_ ["row", "no-gutters", "h-100"]] $ do
          div_ [classes_ ["col-md-8", "h-100"]] $
            let versionParam = case find (\r -> RH.revisionContentUrl r == RH.fileContent f) revisions of
                  Just r  -> "?version=" <> T.takeWhileEnd (/= '/') (render (RH.revisionUri r))
                  Nothing -> ""
            in iframe_ [ src_ (render (RH.fileUri f) <> "/content" <> versionParam)
                       , style_ "width: 100%; height: 100%; border: 0;"
                       ] (toHtml ("" :: Text))
          div_ [classes_ ["col-md-4", "h-100", "overflow-auto", "border-left", "p-3"]] $ do
            div_ [classes_ ["card", "mb-3"]] $ do
              div_ [class_ "card-header"] $ strong_ "Metadata"
              div_ [classes_ ["card-body", "p-0"]] $
                table_ [classes_ ["table", "table-sm", "table-bordered", "mb-0"]] $
                  tbody_ $ do
                    metaRow "File name"     (fromMaybe "" . RH.fileName $ f)
                    metaRow "Size (bytes)"  ((show :: Integer -> Text) . RH.fileSize $ f)
                    metaRow "Title"         (fromMaybe "" . RH.fileTitle $ f)
                    metaRow "Description"   (fromMaybe "" . RH.fileDescription $ f)
                    metaRow "Media Type"    (showMediaType . RH.fileMediaType $ f)
                    metaRow "Created"       (showUTCTime . RH.fileCreatedAt $ f)
                    metaRow "Last Modified" (showUTCTime . RH.fileUpdatedAt $ f)
                    tr_ $ do
                      th_ [classes_ ["align-middle", "text-nowrap", "bg-light"]] "Subjects"
                      td_ $ forM_ (RH.fileSubjects f) $ \uri ->
                        let label = maybe (render uri) Concept.conceptPrefLabel (Concept.lookupConcept concepts uri)
                        in a_ [href_ (render uri), classes_ ["badge", "badge-pill", "badge-info", "mr-1"]] (toHtml label)
            div_ [classes_ ["card", "mb-3"]] $ do
              div_ [class_ "card-header"] $ strong_ "Upload New Content"
              div_ [class_ "card-body"] $
                form_
                  [ method_ "POST"
                  , action_ (render . RH.fileUri $ f)
                  , enctype_ "multipart/form-data"
                  ] $ do
                  div_ [class_ "form-group"] $ do
                    label_ [for_ "file-input"] "Replacement file"
                    input_ [ type_ "file"
                           , name_ "file"
                           , id_ "file-input"
                           , class_ "form-control-file"
                           ]
                  button_ [type_ "submit", classes_ ["btn", "btn-primary", "btn-sm"]] "Submit"
            when (not $ null revisions) $
              div_ [class_ "card"] $ do
                div_ [class_ "card-header"] $ strong_ "Revision History"
                div_ [classes_ ["card-body", "p-0"]] $
                  table_ [classes_ ["table", "table-sm", "table-bordered", "mb-0"]] $ do
                    thead_ $ tr_ $ do
                      th_ "Date"
                      th_ "Size (bytes)"
                    tbody_ $
                      forM_ revisions $ \r -> do
                        let uuid    = T.takeWhileEnd (/= '/') (render (RH.revisionUri r))
                            href    = render (RH.fileUri f) <> "?version=" <> uuid
                            isCurrent = RH.revisionContentUrl r == RH.fileContent f
                        tr_ $ do
                          td_ $ do
                            a_ [href_ href] (toHtml $ showUTCTime (RH.revisionCreated r))
                            when isCurrent $
                              span_ [classes_ ["badge", "badge-primary", "ml-2"]] "current"
                          td_ (toHtml $ (show :: Integer -> Text) (RH.revisionSize r))

    where metaRow :: Monad m => Text -> Text -> HtmlT m ()
          metaRow lbl val = tr_ $ do
            th_ [classes_ ["align-middle", "text-nowrap", "bg-light"]] (toHtml lbl)
            td_ (toHtml val)

  toHtmlRaw = toHtml
