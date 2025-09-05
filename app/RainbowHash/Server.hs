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
import           Text.URI               (URI, render)

import           RainbowHash.App        (AppError, appErrorToString, runApp)
import           RainbowHash.Config     (Config (..))
import           RainbowHash.LinkedData (FileNodeCreateOption (..),
                                         getRecentFiles, putFile)
import           RainbowHash.Servant    (WebIDUserAuth, genAuthServerContext)
import           RainbowHash.User (User, userWebId)
import           RainbowHash.View.File  (File (..))
import           RainbowHash.View.Home  (Home (..))
import           RainbowHash.View.HTML  (HTML)

newtype ServantURI = ServantURI { toURI :: URI }

instance ToHttpApiData ServantURI where
  toUrlPiece = render . toURI

type FilesAPI =
  WebIDUserAuth :>
  (Get '[HTML] Home
    :<|> "files" :> MultipartForm Tmp (MultipartData Tmp)
                 :> Post '[JSON] NoContent)
  :<|> "static" :> Raw

api :: Proxy FilesAPI
api = Proxy

homeHandler :: Config -> User -> Handler Home
homeHandler config user = do
  -- TODO: getRecentfiles should take the User to determine what files the user can see.
  either' <- liftIO $ runApp getRecentFiles config
  case either' of
    Left err          -> throwError $ err500 { errBody = errToLBS err }
    Right recentFiles -> pure $ Home user (File <$> recentFiles)

  where
    errToLBS :: AppError -> LBS.ByteString
    errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

filesHandler
  :: Config
  -> User
  -> MultipartData Tmp
  -> Handler NoContent
filesHandler config user multipartData = do
  case files multipartData of
    [fileData] -> uploadFile fileData (inputs multipartData)
    _ -> throwError (err400 { errBody = "Must supply data for a single file for upload." })

  where uploadFile
          :: FileData Tmp
          -> [Input]
          -> Handler NoContent
        uploadFile fileData fields = do
          let filePath = fdPayload fileData
              maybeFileName = Just $ fdFileName fileData
              maybeTitle = getTitle fields
              maybeDesc = getDescription fields
              maybeMT :: Maybe MediaType
              maybeMT = Nothing
              fileNodeCreateOption :: FileNodeCreateOption
              fileNodeCreateOption = getFileNodeCreationOption fields

          either' <- liftIO $ runApp (putFile filePath (userWebId user) maybeFileName maybeTitle maybeDesc maybeMT fileNodeCreateOption) config
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

        errToLBS :: AppError -> LBS.ByteString
        errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

staticHandler :: Server Raw
staticHandler = serveDirectoryWebApp "static"

server :: Config -> Server FilesAPI
server config = (\authedUser -> homeHandler config authedUser :<|> filesHandler config authedUser) :<|> staticHandler

app :: Config -> Application
app config = serveWithContext api genAuthServerContext (server config)
