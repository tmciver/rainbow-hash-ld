{-# LANGUAGE FlexibleInstances #-}

module Caldron.View.File (File(..)) where

import           Protolude hiding (for_)

import qualified Data.CaseInsensitive as CI

import           Data.Text            as T
import           Data.Text.Encoding   as T
import           Data.Time.Clock      (UTCTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Lucid
import           Lucid.Base           (makeAttribute)
import           Network.HTTP.Media   (MediaType, mainType, subType)
import           Text.URI             (render)

import           Caldron.Concept  (Concept)
import qualified Caldron.Concept  as Concept
import qualified Caldron.File     as RH

data File = File RH.File [Concept]

newtype FileRow = FileRow RH.File

instance ToHtml [File] where
  toHtml [] = pure ()
  toHtml files = do
    let fileRows :: [FileRow]
        fileRows = fmap (\(File f _) -> FileRow f) files
    h2_ "Recent Files"
    table_ [ makeAttribute "border" "1"
           , classes_ ["table", "table-bordered", "table-hover"]
           ] $ do
      thead_ [class_ "thead-dark"] $ do
        tr_ $ do
          th_ "File name"
          th_ "Size (bytes)"
          th_ "Title"
          th_ "Description"
          th_ "Media Type"
          th_ "Created"
          th_ "Last Modified"
      tbody_ (foldMap toHtml fileRows)

  toHtmlRaw = toHtml

instance ToHtml FileRow where
  toHtml (FileRow f) =
    let fileLink = render (RH.fileUri f)
        linkedCell ::
          Applicative m
          => HtmlT m ()
          -> HtmlT m ()
        linkedCell content = td_ $ a_ [href_ fileLink] content
    in tr_ $ do
      linkedCell (toHtml . fromMaybe "" . RH.fileName $ f)
      linkedCell (toHtml . (show :: Integer -> Text) . RH.fileSize $ f)
      linkedCell (toHtml . fromMaybe "" . RH.fileTitle $ f)
      linkedCell (toHtml . fromMaybe "" . RH.fileDescription $ f)
      linkedCell (toHtml . showMediaType . RH.fileMediaType $ f)
      linkedCell (toHtml . showUTCTime . RH.fileCreatedAt $ f)
      linkedCell (toHtml . showUTCTime . RH.fileUpdatedAt $ f)

    where showMediaType :: MediaType -> Text
          showMediaType mt = T.decodeUtf8 . CI.original $ mainType mt <> "/" <> subType mt

          showUTCTime :: UTCTime -> Text
          showUTCTime = T.pack . formatTime defaultTimeLocale "%B %e, %Y %l:%M:%S%p %Z"

  toHtmlRaw = toHtml

instance ToHtml File where
  toHtml (File f concepts) = html_ $ do
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
            iframe_ [ src_ (fileContentSrc f)
                    , style_ "width: 100%; height: 100%; border: 0;"
                    ] (toHtml ("" :: Text))
          div_ [classes_ ["col-md-4", "h-100", "overflow-auto", "border-left", "p-3"]] $ do
            div_ [classes_ ["card", "mb-3"]] $ do
              img_ [ src_ (fileThumbnailSrc f)
                   , class_ "card-img-top"
                   , alt_ "File thumbnail"
                   , makeAttribute "onerror" "this.style.display='none'"
                   ]
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
            div_ [class_ "card"] $ do
              div_ [class_ "card-header"] $ strong_ "Upload New Content"
              div_ [class_ "card-body"] $
                form_
                  [ method_ "POST"
                  , action_ (filePostAction f)
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

    where metaRow :: Monad m => Text -> Text -> HtmlT m ()
          metaRow lbl val = tr_ $ do
            th_ [classes_ ["align-middle", "text-nowrap", "bg-light"]] (toHtml lbl)
            td_ (toHtml val)

          showMediaType :: MediaType -> Text
          showMediaType mt = T.decodeUtf8 . CI.original $ mainType mt <> "/" <> subType mt

          showUTCTime :: UTCTime -> Text
          showUTCTime = T.pack . formatTime defaultTimeLocale "%B %e, %Y %l:%M:%S%p %Z"

          filePostAction :: RH.File -> Text
          filePostAction = render . RH.fileUri

          fileContentSrc :: RH.File -> Text
          fileContentSrc f' = render (RH.fileUri f') <> "/content"

          fileThumbnailSrc :: RH.File -> Text
          fileThumbnailSrc f' = render (RH.fileUri f') <> "/thumbnail"

  toHtmlRaw = toHtml
