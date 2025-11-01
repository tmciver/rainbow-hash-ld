{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.App
  ( App
  , runApp
  ) where

import Protolude

import System.Posix.Files (getFileStatus, fileOwner)
import System.Posix.User (getUserEntryForID, userName)
import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr, logInfoN, logErrorN)
import qualified Data.Set.Ordered as OSet
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.Wreq (defaults, manager, postWith, checkResponse, headWith, header)
import Network.HTTP.Client (Response(responseStatus), ManagerSettings (managerResponseTimeout), defaultManagerSettings, responseTimeoutMicro)
import Network.HTTP.Types (statusIsSuccessful)
import Text.URI (renderStr, URI, mkURI, render, uriAuthority, authHost, unRText, uriPort)
import Control.Lens (set, (.~))
import Control.Monad.Catch (MonadThrow)
import qualified System.Directory as Dir
import System.FSNotify (Event(..), Action, EventIsDirectory(..), withManager, watchDir)
import System.FilePath ((</>), takeFileName)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Default.Class (def)
import           Data.Maybe (fromMaybe)
import           Data.X509 (CertificateChain(..))
import qualified Data.X509 as X509
import qualified Data.X509.File as X509File
import           Data.X509.Validation (getSigned)
import qualified Network.Connection as Conn
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.TLS as TLS

import RainbowHash.CLI.Config (Config(..))
import RainbowHash (Hash)
import RainbowHash.CLI (HttpRead(..), HttpWrite(..), FileSystemRead (..), FileSystemWrite (..), DirectoryWatch(..), AppError(..))
import RainbowHash.EmailAddress (getEmailAddress)
import RainbowHash.Logger (writeLog)

newtype App a = App { unApp :: ExceptT AppError (ReaderT Config IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadThrow, MonadError AppError)

runApp :: App a -> Config -> IO (Either AppError a)
runApp = runReaderT . runExceptT . unApp

instance HttpWrite App where
  postFile fp emailAddress = do
    config <- ask
    let sUri = serverUri config
        url = renderStr sUri

    logInfoN $ "Uploading file at " <> T.pack fp <> " to " <> T.pack url

    let
      -- Assuming Config has fields `clientCertPath :: Maybe FilePath` and
      -- `clientKeyPath :: Maybe FilePath`. These will need to be added to the
      -- Config data type.
      mCertConfig = (,) <$> clientCertPath config <*> clientKeyPath config
      mAuth = uriAuthority sUri
      mHostName = T.unpack . unRText . authHost <$> mAuth
      mPort = BS8.pack . show <$> uriPort sUri

    managerSettings <- case (mCertConfig, mHostName) of
      (Just (certPath, keyPath), Just hostName) -> do
        eCred <- liftIO . try $ do
          certs <- X509File.readSignedObject certPath
          let certChain = CertificateChain $ map (X509.signedObject . getSigned) certs
          [privKey] <- X509File.readKeyFile keyPath -- Assumes one private key in file.
          pure (certChain, privKey)

        case eCred of
          Left (e :: SomeException) -> do
            let errMsg = "Failed to load client certificate: " <> show e
            logErrorN errMsg
            throwError $ PostError errMsg
          Right cred -> do
            let clientParams = TLS.ClientParams
                  { TLS.clientUseMaxFragmentLength = Nothing
                  , TLS.clientServerIdentification = (hostName, fromMaybe "443" mPort)
                  , TLS.clientUseServerNameIndication = True
                  , TLS.clientWantSessionResume = Nothing
                  , TLS.clientShared = def
                  , TLS.clientHooks = def
                  , TLS.clientSupported = def
                  , TLS.clientDebug = def
                  , TLS.clientUseCertificate = Just cred
                  }
                tlsSettings = Conn.TLSSettings clientParams
                settings = TLS.mkManagerSettings tlsSettings Nothing
            pure $ settings { managerResponseTimeout = responseTimeoutMicro 60000000 }
      (Nothing, _) -> throwError $ PostError "Client certificate and/or key path not configured."
      (Just _, Nothing) -> do
        logInfoN "Could not determine hostname from server URI, proceeding without client certificate."
        pure $ defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 60000000 }

    let part = partFile "" fp
        emailBS = T.encodeUtf8 . getEmailAddress $ emailAddress
        opts = defaults & set checkResponse (Just $ \_ _ -> pure ())
                        & manager .~ Left managerSettings
                        & header "From" .~ [emailBS]
    eitherRes <- liftIO $ try $ postWith opts url part
    case eitherRes of
      Left (SomeException e) -> do
        let errMsg = T.pack (displayException e)
        logErrorN $ "Received error from server: " <> errMsg
        throwError $ PostError errMsg
      Right res ->
        if statusIsSuccessful . responseStatus $ res
          then logInfoN ("Success uploading file." :: Text)
          else do
            logErrorN ("Error uploading file." :: Text)
            throwError $ PostError "Non-success HTTP status returned"

hashToUrl
  :: ( MonadThrow m
     , MonadReader Config m
     )
  => Hash
  -> m URI
hashToUrl h = do
  host <- asks serverUri
  mkURI $ render host <> "/blob/" <> h

getFileOwnerUsername :: FilePath -> IO Text
getFileOwnerUsername path = do
  status <- getFileStatus path
  let ownerId = fileOwner status
  userEntry <- getUserEntryForID ownerId
  return . T.pack . userName $ userEntry

instance HttpRead App where
  doesFileExistInStore h = do
    fileUrl <- hashToUrl h
    logInfoN $ "Checking if file exists on server: " <> render fileUrl
    let opts = defaults & set checkResponse (Just $ \_ _ -> pure ()) -- I'm not sure if this is working: still get an exception if server is not up.
                        & manager .~ Left defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 3000000 }
    eitherRes <- liftIO $ try $ headWith opts (T.unpack . render $ fileUrl)
    pure $ case eitherRes of
      Left (SomeException _) -> False
      Right res -> statusIsSuccessful . responseStatus $ res

instance FileSystemRead App where
  readFile = liftIO . BS.readFile
  listDirectory = (OSet.fromList <$>) . liftIO . Dir.listDirectory
  doesFileExist = liftIO . Dir.doesFileExist
  getFileOwner = liftIO . getFileOwnerUsername

instance FileSystemWrite App where
  createDirectory = liftIO . Dir.createDirectoryIfMissing True
  moveToDirectory fp dir = do
    let dest = dir </> takeFileName fp
    logInfoN $ "Moving " <> T.pack fp <> " to " <> T.pack dir
    liftIO $ Dir.renameFile fp dest
  deleteFile fp = do
    logInfoN $ "Deleting file " <> T.pack fp
    liftIO $ Dir.removeFile fp

instance DirectoryWatch App where
  watchDirectory dir action = do
    config <- ask
    liftIO $ withManager $ \mgr -> do
      -- start a watching job (in the background)
      void $ watchDir
        mgr          -- manager
        dir          -- directory to watch
        isFileAdded  -- predicate
        (uploadAction config) -- action

      -- sleep forever (until interrupted)
      forever $ threadDelay 1000000

        where isFileAdded :: Event -> Bool
              isFileAdded Added{} = True
              isFileAdded _ = False

              uploadAction :: Config -> Action
              uploadAction config (Added fp _ IsFile) = do
                e <- runApp (action fp) config
                case e of
                  Left err -> putStrLn $ ("There was an error" :: Text) <> show err
                  _ -> pure ()
              uploadAction _ (Added _ _ IsDirectory) = putStrLn ("Directory added. Ignoring" :: Text)
              uploadAction _ e = putStrLn $ ("Ignoring event: " :: Text) <> show e

instance MonadLogger App where
  monadLoggerLog _ _ level msg = liftIO $ writeLog level (T.decodeUtf8 . fromLogStr . toLogStr $ msg)
