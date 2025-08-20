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
import           RainbowHash.Servant    (WebID, WebIDAuth, genAuthServerContext)
import           RainbowHash.View.File  (File (..))
import           RainbowHash.View.Home  (Home (..))
import           RainbowHash.View.HTML  (HTML)

newtype ServantURI = ServantURI { toURI :: URI }

instance ToHttpApiData ServantURI where
  toUrlPiece = render . toURI

type FilesAPI =
  WebIDAuth :>
  (Get '[HTML] Home
    :<|> "files" :> MultipartForm Tmp (MultipartData Tmp)
                 :> PostCreated '[JSON] (Headers '[Header "Location" ServantURI] NoContent))
  :<|> "static" :> Raw

api :: Proxy FilesAPI
api = Proxy

homeHandler :: Config -> WebID -> Handler Home
homeHandler config _ = do
  -- TODO: getRecentfiles should take the WebID to determine what files the user can see.
  either' <- liftIO $ runApp getRecentFiles config
  case either' of
    Left err          -> throwError $ err500 { errBody = errToLBS err }
    Right recentFiles -> pure $ Home (File <$> recentFiles)

  where
    errToLBS :: AppError -> LBS.ByteString
    errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

filesHandler
  :: Config
  -> WebID
  -> MultipartData Tmp
  -> Handler (Headers '[Header "Location" ServantURI] NoContent)
filesHandler config webId multipartData = do
  case files multipartData of
    [fileData] -> uploadFile webId fileData (inputs multipartData)
    _ -> throwError (err400 { errBody = "Must supply data for a single file for upload." })

  where uploadFile
          :: WebID
          -> FileData Tmp
          -> [Input]
          -> Handler (Headers '[Header "Location" ServantURI] NoContent)
        uploadFile webId' fileData fields = do
          let filePath = fdPayload fileData
              maybeFileName = Just $ fdFileName fileData
              maybeTitle = getTitle fields
              maybeDesc = getDescription fields
              maybeMT :: Maybe MediaType
              maybeMT = Nothing
              fileNodeCreateOption :: FileNodeCreateOption
              fileNodeCreateOption = getFileNodeCreationOption fields

          either' <- liftIO $ runApp (putFile filePath webId' maybeFileName maybeTitle maybeDesc maybeMT fileNodeCreateOption) config
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
        getFileNodeCreationOption = boolToFNCO . maybe False ((== "1") . iValue) . find isFileNodeCreationOption
          where isFileNodeCreationOption :: Input -> Bool
                isFileNodeCreationOption = (== "create-new-node") . iName

                boolToFNCO :: Bool -> FileNodeCreateOption
                boolToFNCO True  = AlwaysCreate
                boolToFNCO False = CreateIfNotExists

        errToLBS :: AppError -> LBS.ByteString
        errToLBS = LBS.fromStrict . encodeUtf8 . appErrorToString

staticHandler :: Server Raw
staticHandler = serveDirectoryWebApp "static"

server :: Config -> Server FilesAPI
server config = (\webId -> homeHandler config webId :<|> filesHandler config webId) :<|> staticHandler

app :: Config -> Application
app config = serveWithContext api genAuthServerContext (server config)
