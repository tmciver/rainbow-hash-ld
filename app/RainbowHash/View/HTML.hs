{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RainbowHash.View.HTML (HTML) where

import           Protolude

import           Lucid
import           Network.HTTP.Media ((//), (/:))
import           Servant            (Accept (..), MimeRender (..))

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml
