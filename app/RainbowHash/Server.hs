{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module RainbowHash.Server (app) where

import Protolude hiding (Handler)

import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import Network.HTTP.Media (MediaType)
import Servant hiding (URI)
import Servant.Multipart
import Text.URI (URI, mkURI, render)

import RainbowHash.App (runApp, appErrorToString, AppError)
import RainbowHash.Config (getConfig)
import RainbowHash.LinkedData (putFile)

newtype ServantURI = ServantURI { toURI :: URI }

instance ToHttpApiData ServantURI where
  toUrlPiece = render . toURI

type FilesAPI = "files" :> MultipartForm Tmp (MultipartData Tmp) :> PostCreated '[JSON] (Headers '[Header "Location" ServantURI] NoContent)

api :: Proxy FilesAPI
api = Proxy

filesServer :: Server FilesAPI
filesServer multipartData = do
  case files multipartData of
    [fileData] -> uploadFile fileData
    _ -> throwError (err400 { errBody = "Must supply data for a single file for upload." })

  where uploadFile :: FileData Tmp -> Handler (Headers '[Header "Location" ServantURI] NoContent)
        uploadFile fileData = do
          let filePath = fdPayload fileData
              maybeFileName = Just $ fdFileName fileData
              maybeMT :: Maybe MediaType
              maybeMT = Nothing
              agentUri :: URI
              agentUri = "http://timmciver.com/me#" & mkURI & fromJust

          config <- liftIO getConfig

          either' <- liftIO $ runApp (putFile filePath agentUri maybeFileName maybeMT) config
          case either' of
            Left err -> throwError $ err500 { errBody = errToLBS err }
            Right uri -> pure $ addHeader (ServantURI uri) NoContent

        errToLBS :: AppError -> LBS.ByteString
        errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

app :: Application
app = serve api filesServer
