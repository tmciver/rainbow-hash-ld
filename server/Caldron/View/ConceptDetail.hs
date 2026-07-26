{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.View.ConceptDetail (ConceptDetailPage (..)) where

import Protolude hiding (for_)

import           Lucid
import qualified Data.Text as T
import           Text.URI  (render)

import Caldron.Concept      (Concept (..), ConceptDetail (..))
import Caldron.User         (User, userName)
import Caldron.View.Common  (navbar, pageFooter)

data ConceptDetailPage = ConceptDetailPage User ConceptDetail

conceptUuid :: Concept -> Text
conceptUuid = T.takeWhileEnd (/= '/') . render . conceptUri

instance ToHtml ConceptDetailPage where
  toHtml (ConceptDetailPage user cd) =
    let c    = cdConcept cd
        uuid = conceptUuid c
    in html_ $ do
      head_ $ do
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        title_ (toHtml (conceptPrefLabel c <> " \x2014 Caldron"))
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css"]
        link_ [rel_ "stylesheet", href_ "/static/style.css"]
        style_ pillCss
      body_ $ do
        navbar (userName user)
        div_ [ classes_ ["container", "mt-4"]
             , id_ "concept-detail"
             , term "data-concept-id" uuid
             ] $ do
          h2_ [class_ "mb-1"] (toHtml (conceptPrefLabel c))
          p_ [class_ "text-muted small mb-4"] (toHtml (render (conceptUri c)))

          div_ [class_ "row"] $ do
            relationSection "broader"  "Broader"  "(more general)"  (cdBroader  cd)
            relationSection "narrower" "Narrower" "(more specific)" (cdNarrower cd)

          p_ [classes_ ["text-muted", "small", "mt-2"]] $
            "Adding a broader concept here will automatically make this concept \
            \appear as narrower of that concept."

        pageFooter
        script_ [src_ "/static/concept-hierarchy.js"] ("" :: Text)
  toHtmlRaw = toHtml

relationSection :: Monad m => Text -> Text -> Text -> [Concept] -> HtmlT m ()
relationSection rel heading qualifier concepts =
  div_ [class_ "col-md-6 mb-4"] $ do
    h5_ [class_ "mb-3"] $ do
      toHtml heading
      " "
      span_ [classes_ ["text-muted", "small", "font-weight-normal"]] (toHtml qualifier)
    div_ [id_ (rel <> "-pills"), class_ "mb-2"] $
      forM_ concepts (conceptPill rel)
    div_ [class_ "input-group"] $ do
      div_ [class_ "position-relative", style_ "flex:1"] $ do
        input_ [ type_ "text"
               , id_ (rel <> "-search")
               , class_ "form-control"
               , placeholder_ "Search for a concept..."
               , autocomplete_ "off"
               ]
        div_ [ id_ (rel <> "-suggestions")
             , class_ "list-group"
             , style_ "position:absolute;z-index:1000;width:100%;display:none"
             ] ""
      div_ [class_ "input-group-append"] $
        button_ [ type_ "button"
                , id_ ("add-" <> rel <> "-btn")
                , classes_ ["btn", "btn-secondary"]
                , disabled_ ""
                ] "Add"

conceptPill :: Monad m => Text -> Concept -> HtmlT m ()
conceptPill rel c =
  span_ [classes_ ["badge", "badge-secondary", "mr-1", "mb-1", "concept-pill"]] $ do
    a_ [href_ (render (conceptUri c)), class_ "text-white"] (toHtml (conceptPrefLabel c))
    button_ [ type_ "button"
            , class_ "concept-pill-remove remove-relation-btn"
            , term "data-rel" rel
            , term "data-related-id" (conceptUuid c)
            ] "\xd7"

pillCss :: Text
pillCss =
  ".concept-pill { display:inline-flex; align-items:center; }\
  \.concept-pill-remove { background:none; border:none; color:inherit; cursor:pointer;\
  \ font-size:0.9em; line-height:1; margin-left:4px; opacity:0.8; padding:0; }\
  \.concept-pill-remove:hover { opacity:1; }"
