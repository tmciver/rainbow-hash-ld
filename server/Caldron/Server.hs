{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts     #-}

module Caldron.Server (app) where

import           Protolude              hiding (Handler)

import           Control.Monad.Logger          (LogLevel(LevelInfo, LevelError))
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Client    (defaultManagerSettings, newManager,
                                         parseRequest)
import           Network.HTTP.Media     (MediaType)
import           Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..),
                                            defaultOnExc, waiProxyTo)
import           Network.HTTP.Types     (status200, status400, status404, status500)
import           Network.Wai            (responseLBS, rawPathInfo, pathInfo, rawQueryString, queryString)
import           Servant                hiding (URI)
import           Servant.Multipart
import           Text.URI               (URI, mkURI, render)

import RainbowHash.Logger            (writeLog)
import Caldron.App        (AppError(FileError), appErrorToString, runApp)
import qualified Caldron.HSPARQL as HSPARQL
import qualified Caldron.File as RH
import qualified Caldron.App as App
import Caldron.Config     (Config (..))
import Caldron.LinkedData (FileNodeCreateOption (..), getFile,
                                         getRecentFiles, putFile, fileErrorToText)
import Caldron.Profile    (ProfileCache)
import Caldron.Thumbnail  (getThumbnail)
import Caldron.Servant    (WebIDUserAuth, genAuthServerContext)
import Caldron.User (User, userWebId)
import Caldron.View.File  (File (..))
import Caldron.View.Home  (Home (..))
import Caldron.View.HTML  (HTML)
import Caldron.EmailAddress (EmailAddress(..))
import Caldron.Concept (Concept)

type FilesAPI =
  WebIDUserAuth :>
  (Get '[HTML] Home
    :<|> "files" :> Header "Host" Text
                 :> Header "From" Text
                 :> MultipartForm Tmp (MultipartData Tmp)
                 :> PostNoContent
    :<|> "file"  :>
      (
           Header "Host" Text
        :> Capture "fileId" Text
        :> "content"
        :> Raw
      :<|>
           Header "Host" Text
        :> Capture "fileId" Text
        :> Get '[HTML] File
      :<|>
           -- This endpoint is for use by the web form used to update a file as
           -- it responds with a redirect;
           -- programmatic clients should use the below PUT endpoint
           Header "Host" Text
        :> Header "From" Text
        :> Capture "fileId" Text
        :> MultipartForm Tmp (MultipartData Tmp)
        :> PostNoContent
      :<|>
           Header "Host" Text
        :> Header "From" Text
        :> Capture "fileId" Text
        :> MultipartForm Tmp (MultipartData Tmp)
        :> PutNoContent
      :<|>
           Header "Host" Text
        :> Capture "fileId" Text
        :> "thumbnail"
        :> Raw
      )
    :<|> "concepts" :> QueryParam "q" Text :> Get '[JSON] [Concept]
  )
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
    Right recentFiles -> pure $ Home user ((\f -> File f []) <$> recentFiles)

getFileHandler :: Config -> User -> Maybe Text -> Text -> Handler File
getFileHandler config _ mHost fileId =
  let defaultHost = "example.com"
      host = fromMaybe defaultHost $ (preferredHost config) <|> mHost
      uriText = "https://" <> host <> "/file/" <> fileId
  in case mkURI uriText of
    Nothing -> throwError $ err400 { errBody = "Could not construct a valid URI for file." }
    Just fileUri -> do
      liftIO $ writeLog LevelInfo $ "Getting file: " <> uriText
      either' <- liftIO $ runApp (getFile fileUri) config
      case either' of
        Left err             -> throwError $ err500 { errBody = errToLBS err }
        Right Nothing        -> throwError err404
        Right (Just rhFile) -> do
          concepts <- liftIO $ HSPARQL.getConceptsByUris (sparqlEndpoint config) (RH.fileSubjects rhFile)
          pure $ File rhFile concepts

webIdFromEmail :: EmailAddress -> Config -> Maybe URI
webIdFromEmail email = Map.lookup email . webIdMap

getWebIdForEmail :: Config -> EmailAddress -> Handler (Maybe URI)
getWebIdForEmail config' email = do
  case webIdFromEmail email config' of
    Nothing -> do
      let msg = "No WebId associated with email address " <> getEmailAddress email
      liftIO $ writeLog LevelError msg
      throwError err500 { errBody = LBS.fromStrict $ encodeUtf8 msg }
    Just authorUri -> do
      liftIO $ writeLog LevelInfo $ "Found WebID " <> render authorUri <> " associated with email " <> getEmailAddress email
      pure $ Just authorUri

updateFileHandler
  :: (URI -> Handler a)
  -> Config
  -> User
  -> Maybe Text -- ^Host header
  -> Maybe Text -- ^From header
  -> Text
  -> MultipartData Tmp
  -> Handler NoContent
updateFileHandler response config user mHost mFrom fileId multipartData =
  let defaultHost = "example.com"
      host = fromMaybe defaultHost $ (preferredHost config) <|> mHost
      uriText = "https://" <> host <> "/file/" <> fileId
  in case mkURI uriText of
    Nothing -> throwError $ err400 { errBody = "Could not construct a valid URI for file." }
    Just fileUri -> do
      liftIO $ writeLog LevelInfo $ "PUT file: " <> uriText
      case files multipartData of
        [fileData] -> do
          let filePath = fdPayload fileData
              mMediaType :: Maybe MediaType
              mMediaType = Nothing

          -- See if there's an on-behalf-of user
          mOnBehalfOf <- maybe (pure Nothing) (getWebIdForEmail config) (mFrom <&> EmailAddress)

          eitherRes <- liftIO $ runApp (App.updateFileContent host fileUri filePath (userWebId user) mOnBehalfOf mMediaType) config
          void $ case eitherRes of
            -- TODO: dispatch on AppError values to determine what error to throw.
            Left appError -> throwError (err400 { errBody = errToLBS appError })
            Right _ -> response fileUri
          pure NoContent
        _ -> throwError (err400 { errBody = "Must supply data for a single file for upload." })

postFileHandler
  :: Config
  -> User
  -> Maybe Text
  -> Maybe Text -- ^From header
  -> Text
  -> MultipartData Tmp
  -> Handler NoContent
postFileHandler =
  let resp fileUri = throwError err303 { errHeaders = [("Location", encodeUtf8 . render $ fileUri)] }
  in updateFileHandler resp

putFileHandler
  :: Config
  -> User
  -> Maybe Text -- ^Host header
  -> Maybe Text -- ^From header
  -> Text
  -> MultipartData Tmp
  -> Handler NoContent
putFileHandler =
  let resp = const $ pure NoContent
  in updateFileHandler resp

filesHandler
  :: Config
  -> User
  -> Maybe Text
  -> Maybe Text
  -> MultipartData Tmp
  -> Handler NoContent
filesHandler config user mHost mFrom multipartData = do
  case (files multipartData, mHost) of
    ([fileData], Just host) -> uploadFile host mFrom fileData (inputs multipartData)
    (_, Nothing) -> throwError (err400 { errBody = "HOST header not set. Consider configuring one using the `default-host` configuration option." }) 
    _ -> throwError (err400 { errBody = "Must supply data for a single file for upload." })

  where uploadFile
          :: Text
          -> Maybe Text
          -> FileData Tmp
          -> [Input]
          -> Handler NoContent
        uploadFile host' mFrom' fileData fields = do
          let filePath = fdPayload fileData
              maybeFileName = let fn = fdFileName fileData
                in if fn == "\"\""
                   then Nothing
                   else Just fn
              maybeTitle = getTitle fields
              maybeDesc = getDescription fields
              subjects = mapMaybe mkURI $ getSubjects fields
              newConceptLabels = getNewConceptLabels fields
              maybeMT :: Maybe MediaType
              maybeMT = Nothing
              fileNodeCreateOption :: FileNodeCreateOption
              fileNodeCreateOption = getFileNodeCreationOption fields

          -- See if there's an on-behalf-of user
          mAuthorUri <- maybe (pure Nothing) (getWebIdForEmail config) (mFrom' <&> EmailAddress)

          -- Mint URIs and persist any newly created concepts
          newConceptUris <- liftIO $ do
            results <- mapM (HSPARQL.mintAndCreateConcept (sparqlEndpoint config) host') newConceptLabels
            let (errs, uris) = partitionEithers results
            forM_ errs $ \err -> writeLog LevelError (HSPARQL.sparqlErrorToText err)
            pure uris

          let allSubjects = subjects <> newConceptUris

          either' <- liftIO $ runApp (putFile filePath host' (userWebId user) mAuthorUri maybeFileName maybeTitle maybeDesc allSubjects maybeMT fileNodeCreateOption) config
          case either' of
            Left err  -> (liftIO $ writeLog LevelError $ appErrorToString err) >> (throwError $ err500 { errBody = errToLBS err })
            Right (Left err) -> (liftIO $ writeLog LevelError $ fileErrorToText err) >> (throwError $ err400 { errBody = errToLBS $ FileError err })
            Right _ -> throwError err303 { errHeaders = [("Location", "/")] }

        getTitle :: [Input] -> Maybe Text
        getTitle = (<&> iValue) . find isTitle
          where isTitle :: Input -> Bool
                isTitle = (== "title") . iName

        getDescription :: [Input] -> Maybe Text
        getDescription = (<&> iValue) . find isDescription
          where isDescription :: Input -> Bool
                isDescription = (== "description") . iName

        getSubjects :: [Input] -> [Text]
        getSubjects = fmap iValue . filter ((== "subject") . iName)

        getNewConceptLabels :: [Input] -> [Text]
        getNewConceptLabels = fmap iValue . filter ((== "new-concept") . iName)

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

fileContentHandler :: Config -> User -> Maybe Text -> Text -> Tagged Handler Application
fileContentHandler config _ mHost fileId = Tagged $ \req respond' -> do
  let defaultHost = "example.com"
      host = fromMaybe defaultHost $ (preferredHost config) <|> mHost
      uriText = "https://" <> host <> "/file/" <> fileId
  case mkURI uriText of
    Nothing -> respond' $ responseLBS status400 [] "Could not construct a valid URI for file."
    Just fileUri -> do
      either' <- runApp (getFile fileUri) config
      case either' of
        Left _          -> respond' $ responseLBS status500 [] "Error fetching file metadata."
        Right Nothing   -> respond' $ responseLBS status404 [] "File not found."
        Right (Just rhFile) -> do
          let contentUrl = T.unpack (render (RH.fileContent rhFile))
          writeLog LevelInfo $ render (RH.fileContent rhFile)
          mgr <- newManager defaultManagerSettings
          httpReq <- parseRequest contentUrl
          let dest   = ProxyDest (HC.host httpReq) (HC.port httpReq)
              blob   = HC.path httpReq
              modReq = req { rawPathInfo    = blob
                           , pathInfo       = filter (not . T.null) . T.splitOn "/" . decodeUtf8 $ blob
                           , rawQueryString = ""
                           , queryString    = []
                           }
          waiProxyTo (\_ -> pure $ WPRModifiedRequest modReq dest) defaultOnExc mgr req respond'

thumbnailCacheDir :: FilePath
thumbnailCacheDir = "data/thumbnails"

thumbnailHandler :: Config -> User -> Maybe Text -> Text -> Tagged Handler Application
thumbnailHandler config _ mHost fileId = Tagged $ \_ respond' -> do
  let defaultHost = "example.com"
      host = fromMaybe defaultHost $ preferredHost config <|> mHost
      uriText = "https://" <> host <> "/file/" <> fileId
  case mkURI uriText of
    Nothing -> respond' $ responseLBS status400 [] "Invalid file URI."
    Just fileUri -> do
      either' <- runApp (getFile fileUri) config
      case either' of
        Left _              -> respond' $ responseLBS status500 [] "Error fetching file metadata."
        Right Nothing       -> respond' $ responseLBS status404 [] "File not found."
        Right (Just rhFile) -> do
          let contentUrl = render (RH.fileContent rhFile)
          mThumb <- getThumbnail thumbnailCacheDir fileId contentUrl
          case mThumb of
            Nothing    -> respond' $ responseLBS status404 [] "Thumbnail not available for this file type."
            Just thumb -> respond' $ responseLBS status200
              [("Content-Type", "image/jpeg")]
              (LBS.fromStrict thumb)

conceptsHandler :: Config -> Maybe Text -> Handler [Concept]
conceptsHandler config mQ = liftIO $ maybe (pure []) (HSPARQL.searchConcepts (sparqlEndpoint config)) mQ

staticHandler :: Server Raw
staticHandler = serveDirectoryWebApp "static"

server :: Config -> Server FilesAPI
server config = (\authedUser -> homeHandler config authedUser
                  :<|> filesHandler config authedUser
                  :<|> (fileContentHandler config authedUser
                        :<|>
                        getFileHandler config authedUser
                        :<|>
                        -- This handler is for use by the web form used to update a file as
                        -- it responds with a redirect;
                        -- PUTs from programmatic clients will use the below PUT handler
                        postFileHandler config authedUser
                        :<|>
                        putFileHandler config authedUser
                        :<|>
                        thumbnailHandler config authedUser)
                  :<|> conceptsHandler config)
                :<|> staticHandler

app :: Config -> ProfileCache -> Application
app config cache = serveWithContext api (genAuthServerContext cache) (server config)
