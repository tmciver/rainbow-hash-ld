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
import RainbowHash.LinkedData (getRecentFiles, putFile)
import RainbowHash.View.File (File(..))
import RainbowHash.View.Home (Home(..))
import RainbowHash.View.HTML (HTML)

newtype ServantURI = ServantURI { toURI :: URI }

instance ToHttpApiData ServantURI where
  toUrlPiece = render . toURI

type FilesAPI = Get '[HTML] Home
           :<|> "files" :> MultipartForm Tmp (MultipartData Tmp)
                        :> PostCreated '[JSON] (Headers '[Header "Location" ServantURI] NoContent)

api :: Proxy FilesAPI
api = Proxy

homeHandler :: Handler Home
homeHandler = do
  config <- liftIO getConfig
  either' <- liftIO $ runApp getRecentFiles config
  case either' of
    Left err -> throwError $ err500 { errBody = errToLBS err }
    Right recentFiles -> pure $ Home (File <$> recentFiles)

  where
    errToLBS :: AppError -> LBS.ByteString
    errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

filesHandler
  :: MultipartData Tmp
  -> Handler (Headers '[Header "Location" ServantURI] NoContent)
filesHandler multipartData = do
  case files multipartData of
    [fileData] -> uploadFile fileData (inputs multipartData)
    _ -> throwError (err400 { errBody = "Must supply data for a single file for upload." })

  where uploadFile
          :: FileData Tmp
          -> [Input]
          -> Handler (Headers '[Header "Location" ServantURI] NoContent)
        uploadFile fileData fields = do
          let filePath = fdPayload fileData
              maybeFileName = Just $ fdFileName fileData
              maybeTitle = getTitle fields
              maybeMT :: Maybe MediaType
              maybeMT = Nothing
              -- FIXME: agentUri should come from client
              agentUri :: URI
              agentUri = "http://timmciver.com/me#" & mkURI & fromJust

          config <- liftIO getConfig

          either' <- liftIO $ runApp (putFile filePath agentUri maybeFileName maybeTitle maybeMT) config
          case either' of
            Left err -> throwError $ err500 { errBody = errToLBS err }
            Right uri -> pure $ addHeader (ServantURI uri) NoContent

        getTitle :: [Input] -> Maybe Text
        getTitle = (<&> iValue) . find isTitle
          where isTitle :: Input -> Bool
                isTitle = (== "title") . iName

        errToLBS :: AppError -> LBS.ByteString
        errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

server :: Server FilesAPI
server = homeHandler :<|> filesHandler

app :: Application
app = serve api server
