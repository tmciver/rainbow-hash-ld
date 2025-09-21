{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module RainbowHash.Server (app) where

import           Protolude              hiding (Handler)

import qualified Data.ByteString.Lazy   as LBS
import           Network.HTTP.Media     (MediaType)
import           Servant                hiding (URI)
import           Servant.Multipart
import           Text.URI               (mkURI)

import           RainbowHash.App        (AppError, appErrorToString, runApp)
import           RainbowHash.Config     (Config (..))
import           RainbowHash.LinkedData (FileNodeCreateOption (..), getFile,
                                         getRecentFiles, putFile)
import           RainbowHash.Servant    (WebIDUserAuth, genAuthServerContext)
import           RainbowHash.User (User, userWebId)
import           RainbowHash.View.File  (File (..))
import           RainbowHash.View.Home  (Home (..))
import           RainbowHash.View.HTML  (HTML)

type FilesAPI =
  WebIDUserAuth :>
  (Get '[HTML] Home
    :<|> "files" :> Header "Host" Text
                 :> MultipartForm Tmp (MultipartData Tmp)
                 :> Post '[JSON] NoContent
    :<|> "file" :> Capture "fileId" Text :> Get '[HTML] File)
  :<|> "static" :> Raw

api :: Proxy FilesAPI
api = Proxy

errToLBS :: AppError -> LBS.ByteString
errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

homeHandler :: Config -> User -> Handler Home
homeHandler config user = do
  -- TODO: getRecentfiles should take the User to determine what files the user can see.
  either' <- liftIO $ runApp getRecentFiles config
  case either' of
    Left err          -> throwError $ err500 { errBody = errToLBS err }
    Right recentFiles -> pure $ Home user (File <$> recentFiles)

fileHandler :: Config -> User -> Text -> Handler File
fileHandler config _ fileId =
  case mkURI fileId of
    Nothing -> throwError $ err400 { errBody = "Invalid file identifier." }
    Just fileUri -> do
      either' <- liftIO $ runApp (getFile fileUri) config
      case either' of
        Left err          -> throwError $ err500 { errBody = errToLBS err }
        Right Nothing     -> throwError err404
        Right (Just file) -> pure $ File file

filesHandler
  :: Config
  -> User
  -> Maybe Text
  -> MultipartData Tmp
  -> Handler NoContent
filesHandler config user mHost multipartData = do
  case files multipartData of
    [fileData] ->
      -- Use either the host provided by the user or the one provied in the Host
      -- header with a preference for the user-specified one. If neither is
      -- present, use defaultHost
      let defaultHost = "example.com"
          host = fromMaybe defaultHost $ (preferredHost config) <|> mHost
      in uploadFile host fileData (inputs multipartData)
    _ -> throwError (err400 { errBody = "Must supply data for a single file for upload." })

  where uploadFile
          :: Text
          -> FileData Tmp
          -> [Input]
          -> Handler NoContent
        uploadFile host' fileData fields = do
          let filePath = fdPayload fileData
              maybeFileName = Just $ fdFileName fileData
              maybeTitle = getTitle fields
              maybeDesc = getDescription fields
              maybeMT :: Maybe MediaType
              maybeMT = Nothing
              fileNodeCreateOption :: FileNodeCreateOption
              fileNodeCreateOption = getFileNodeCreationOption fields

          either' <- liftIO $ runApp (putFile filePath host' (userWebId user) maybeFileName maybeTitle maybeDesc maybeMT fileNodeCreateOption) config
          case either' of
            Left err  -> throwError $ err500 { errBody = errToLBS err }
            Right _ -> throwError err303 { errHeaders = [("Location", "/")] }

        getTitle :: [Input] -> Maybe Text
        getTitle = (<&> iValue) . find isTitle
          where isTitle :: Input -> Bool
                isTitle = (== "title") . iName

        getDescription :: [Input] -> Maybe Text
        getDescription = (<&> iValue) . find isDescription
          where isDescription :: Input -> Bool
                isDescription = (== "description") . iName

        getFileNodeCreationOption :: [Input] -> FileNodeCreateOption
        getFileNodeCreationOption = boolToFNCO . maybe False (isEnabled . iValue) . find isFileNodeCreationOption
          where isFileNodeCreationOption :: Input -> Bool
                isFileNodeCreationOption = (== "create-new-node") . iName

                isEnabled :: Text -> Bool
                isEnabled "1" = True
                isEnabled "on" = True
                isEnabled _ = False

                boolToFNCO :: Bool -> FileNodeCreateOption
                boolToFNCO True  = AlwaysCreate
                boolToFNCO False = CreateIfNotExists

staticHandler :: Server Raw
staticHandler = serveDirectoryWebApp "static"

server :: Config -> Server FilesAPI
server config = (\authedUser -> homeHandler config authedUser :<|> filesHandler config authedUser :<|> fileHandler config authedUser) :<|> staticHandler

app :: Config -> Application
app config = serveWithContext api genAuthServerContext (server config)
