{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module RainbowHash.HTTPClient
  ( putFile
  , HTTPClientError(..)
  , mapError
  , httpClientErrorToString
  , postToSPARQL
  ) where

import Protolude

import Control.Monad.Catch (MonadMask)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.RDF (Rdf, RDF, hWriteRdf, TurtleSerializer(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Request, Response, parseRequest, newManager, defaultManagerSettings, responseHeaders, httpNoBody, httpLbs, responseStatus, responseBody, requestHeaders, requestBody, RequestBody(RequestBodyBS))
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileSource)
import Network.HTTP.Types (Header, hLocation, Status, statusIsSuccessful)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Text.URI (URI, mkURI, render)

data HTTPClientError
  = HeaderError HeaderError
  | ResponseError Status Text
  | RequestError Text
  deriving (Show)

putFile
  :: ( MonadError HTTPClientError m
     , MonadIO m
     )
  => URI
  -> FilePath
  -> m URI
putFile blobStoreUrl fp = do
  mgr <- liftIO $ newManager defaultManagerSettings
  req <- liftIO $ mkPostFileRequest blobStoreUrl fp
  resp <- liftIO $ httpNoBody req mgr
  checkStatus resp
  mapError HeaderError (getLocationHeader resp)

mapError
  :: MonadError e' m
  => (e -> e')
  -> ExceptT e m a
  -> m a
mapError f ex = do
  either' <- runExceptT ex
  case either' of
    Left e -> throwError (f e)
    Right v -> pure v

mkPostFileRequest :: URI -> FilePath -> IO Request
mkPostFileRequest blobStorageUrl fp =
  -- partFileSource sends the filename as part of the payload; no need to send it explicitly.
  parseRequest (T.unpack $ "POST " <> render blobStorageUrl) >>= formDataBody [partFileSource "file" fp]

data HeaderError
  = HeaderNotFound
  | UTF8DecodeError UnicodeException
  | URIImportError Text
  deriving (Show)

headerErrorToString :: HeaderError -> Text
headerErrorToString HeaderNotFound = "The Location header was not found."
headerErrorToString (UTF8DecodeError ue) = "URI decode error: " <> show ue
headerErrorToString (URIImportError t) = "URI import error: " <> t

httpClientErrorToString :: HTTPClientError -> Text
httpClientErrorToString (HeaderError he) = headerErrorToString he
httpClientErrorToString (ResponseError status msg) = "HTTP response error. Status code: " <> show status <> ", message: " <> msg
httpClientErrorToString (RequestError reqText) = "Could not create a HTTP request from \"" <> reqText <> "\""

checkStatusWithTextMessage
  :: MonadError HTTPClientError m
  => (a -> Text)
  -> Response a
  -> m ()
checkStatusWithTextMessage toText resp = do
  let status = responseStatus resp
      isSuccess = statusIsSuccessful status
  if isSuccess
    then pure ()
    else let msg = T.take 100 . toText . responseBody $ resp
         in throwError $ ResponseError status msg

checkStatus
  :: MonadError HTTPClientError m
  => Response a
  -> m ()
checkStatus = checkStatusWithTextMessage (const "No response body provided.")

getLocationHeader
  :: MonadError HeaderError m
  => Response a
  -> m URI
getLocationHeader = findLocationVal >=> locationToURI
  where findLocationVal :: MonadError HeaderError m => Response a -> m ByteString
        findLocationVal resp =
          let maybeHeader = find isLocation . responseHeaders $ resp
          in case maybeHeader of
            Just (_, loc) -> pure loc
            Nothing -> throwError HeaderNotFound 

        isLocation :: Header -> Bool
        isLocation (hdr, _) = hdr == hLocation

        locationToURI :: MonadError HeaderError m => ByteString -> m URI
        locationToURI bs = do
          t <- case T.decodeUtf8' bs of
            Right t' -> pure t'
            Left uErr -> throwError $ UTF8DecodeError uErr
          case mkURI t of
            Just url -> pure url
            Nothing -> throwError $ URIImportError t

postToSPARQL
  :: ( Rdf a
     , MonadIO m
     , MonadMask m
     , MonadError HTTPClientError m
     )
  => URI -- ^SPARQL endpoint (Graph Store Protocol)
  -> RDF a -- ^the graph to send
  -> m ()
postToSPARQL gspUri graph = do
  e <- withSystemTempFile "rainbow-hash-" postFileToSPARQL
  either throwError pure e

  where mkSPARQLRequest
          :: ( MonadError HTTPClientError m
             , MonadIO m
             )
          => URI
          -> FilePath
          -> m Request
        mkSPARQLRequest gspUri' fp = do
          -- initialize the Request object
          let reqText = "POST " <> T.unpack (render gspUri') <> "/data?default"

          req <- case parseRequest reqText of
            Just r -> pure r
            Nothing -> throwError $ RequestError $ "Could not create Request from text \"" <> T.pack reqText <> "\""

          -- update the headers
          let req' = req { requestHeaders = ("Content-Type", "text/turtle") : requestHeaders req }

          -- set the body
          bs <- fp & BS.readFile
                   & liftIO
          let req'' = req' { requestBody = RequestBodyBS bs }

          pure req''

        postFileToSPARQL
          :: ( MonadIO m
             , MonadMask m
             )
          => FilePath
          -> Handle
          -> m (Either HTTPClientError ())
        postFileToSPARQL fp h = do

          -- write the rdf turtle to the temp file handle
          let turtleSerializer = TurtleSerializer Nothing mempty
          liftIO $ hWriteRdf turtleSerializer h graph

          -- close the handle so that it can be later opened by http-client
          liftIO $ hClose h

          -- make the request to the Graph Store Protocol endpoint
          mgr <- liftIO $ newManager defaultManagerSettings
          (runExceptT $ mkSPARQLRequest gspUri fp)
            >>= (\eitherReq -> case eitherReq of
                    Left err -> pure $ Left err
                    Right req -> liftIO (httpLbs req mgr >>= responseToEither)
                )

        responseToEither
          :: Applicative m
          => Response LBS.ByteString
          -> m (Either HTTPClientError ())
        responseToEither = pure . checkStatusWithTextMessage (T.take 100 . T.decodeUtf8 . LBS.toStrict)
