{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module RainbowHash.Server (app) where

import           Protolude              hiding (Handler)

import qualified Data.ByteString.Lazy   as LBS
import           Data.Maybe             (fromJust)
import           Network.HTTP.Media     (MediaType)
import           Servant                hiding (URI)
import           Servant.Multipart
import qualified Text.URI               as URI
import           Text.URI               (URI, mkURI, render)

import           RainbowHash.App        (AppError, appErrorToString, runApp)
import           RainbowHash.Config     (getConfig)
import           RainbowHash.LinkedData (FileNodeCreateOption (..),
                                         getRecentFiles, putFile)
import           RainbowHash.Servant    (WebID, WebIDAuth, genAuthServerContext)
import           RainbowHash.View.File  (File (..))
import           RainbowHash.View.Home  (Home (..))
import           RainbowHash.View.HTML  (HTML)

newtype ServantURI = ServantURI { toURI :: URI }

instance ToHttpApiData ServantURI where
  toUrlPiece = render . toURI

type FilesAPI = WebIDAuth :> Get '[HTML] Home
           :<|> "files" :> MultipartForm Tmp (MultipartData Tmp)
                        :> PostCreated '[JSON] (Headers '[Header "Location" ServantURI] NoContent)
           :<|> "static" :> Raw

api :: Proxy FilesAPI
api = Proxy

-- newtype ClientCert = ClientCert Text

homeHandler :: WebID -> Handler Home
homeHandler webID = do
  liftIO $ putStrLn $ ("WebID is: " :: Text) <> URI.render webID
  config <- liftIO getConfig
  either' <- liftIO $ runApp getRecentFiles config
  case either' of
    Left err          -> throwError $ err500 { errBody = errToLBS err }
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
              maybeDesc = getDescription fields
              maybeMT :: Maybe MediaType
              maybeMT = Nothing
              fileNodeCreateOption :: FileNodeCreateOption
              fileNodeCreateOption = getFileNodeCreationOption fields
              -- FIXME: agentUri should come from client
              agentUri :: URI
              agentUri = "http://timmciver.com/me#" & mkURI & fromJust

          config <- liftIO getConfig

          either' <- liftIO $ runApp (putFile filePath agentUri maybeFileName maybeTitle maybeDesc maybeMT fileNodeCreateOption) config
          case either' of
            Left err  -> throwError $ err500 { errBody = errToLBS err }
            Right uri -> pure $ addHeader (ServantURI uri) NoContent

        getTitle :: [Input] -> Maybe Text
        getTitle = (<&> iValue) . find isTitle
          where isTitle :: Input -> Bool
                isTitle = (== "title") . iName

        getDescription :: [Input] -> Maybe Text
        getDescription = (<&> iValue) . find isDescription
          where isDescription :: Input -> Bool
                isDescription = (== "description") . iName

        getFileNodeCreationOption :: [Input] -> FileNodeCreateOption
        getFileNodeCreationOption = boolToFNCO . fromMaybe False . (<&> (== "1") . iValue) . find isFileNodeCreationOption
          where isFileNodeCreationOption :: Input -> Bool
                isFileNodeCreationOption = (== "create-new-node") . iName

                boolToFNCO :: Bool -> FileNodeCreateOption
                boolToFNCO True  = AlwaysCreate
                boolToFNCO False = CreateIfNotExists

        errToLBS :: AppError -> LBS.ByteString
        errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

staticHandler :: Server Raw
staticHandler = serveDirectoryWebApp "static"

server :: Server FilesAPI
server = homeHandler :<|> filesHandler :<|> staticHandler

app :: Application
app = serveWithContext api genAuthServerContext server
