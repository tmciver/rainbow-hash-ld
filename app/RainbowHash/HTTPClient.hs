{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module RainbowHash.HTTPClient
  ( putFile
  , HTTPClientError(..)
  , mapError
  , httpClientErrorToString
  ) where

import Protolude

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Request, Response, parseRequest, newManager, defaultManagerSettings, responseHeaders, httpNoBody, responseStatus)
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileSource)
import Network.HTTP.Types (Header, hLocation, Status, statusIsSuccessful)
import Text.URI (URI, mkURI, render)

data HTTPClientError
  = HeaderError HeaderError
  | ResponseError Status
  deriving (Show)

putFile
  :: ( MonadError HTTPClientError m
     , MonadIO m
     )
  => URI
  -> FilePath
  -> m URI
putFile blobStoreUrl fp = do
  liftIO $ putStrLn $ "Putting " <> T.pack fp <> " in the store at " <> render blobStoreUrl
  mgr <- liftIO $ newManager defaultManagerSettings
  req <- liftIO $ mkPutFileRequest blobStoreUrl fp
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

mkPutFileRequest :: URI -> FilePath -> IO Request
mkPutFileRequest blobStorageUrl fp =
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
httpClientErrorToString (ResponseError status) = "HTTP response error. Status code: " <> show status

checkStatus :: MonadError HTTPClientError m => Response a -> m ()
checkStatus resp = do
  let status = responseStatus resp
      isSuccess = statusIsSuccessful status
  if isSuccess
    then pure ()
    else throwError $ ResponseError status

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
