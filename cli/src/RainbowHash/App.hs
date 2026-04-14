{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
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
import Network.Wreq (defaults, manager, postWith, checkResponse, header)
import Network.HTTP.Client (Response(responseStatus), ManagerSettings (managerResponseTimeout), responseTimeoutMicro)
import Network.HTTP.Types (statusIsSuccessful)
import Text.URI (renderStr, uriAuthority, authHost, unRText, Authority(..), unRText)
import Control.Lens (set, (.~))
import Control.Monad.Catch (MonadThrow)
import qualified System.Directory as Dir
import System.FSNotify (Event(..), Action, EventIsDirectory(..), withManager, watchDir)
import System.FilePath ((</>), takeFileName)
import qualified Data.ByteString as BS
import           Data.Default (def)
import           Data.X509 (CertificateChain(..), PrivKey)
import qualified Data.X509 as X509
import qualified Data.X509.File as X509File
import Network.Connection (TLSSettings(..))
import qualified Network.HTTP.Client.TLS as TLS
import Network.TLS (defaultParamsClient, clientHooks, onCertificateRequest, onServerCertificate)

import RainbowHash.CLI.Config (Config(..))
import RainbowHash.CLI (HttpWrite(..), FileSystemRead (..), FileSystemWrite (..), DirectoryWatch(..), AppError(..))
import RainbowHash.EmailAddress (getEmailAddress)
import RainbowHash.Logger (writeLog)

newtype App a = App { unApp :: ExceptT AppError (ReaderT Config IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadThrow, MonadError AppError)

runApp :: App a -> Config -> IO (Either AppError a)
runApp = runReaderT . runExceptT . unApp

-- Helper function to read PEM file and extract certificate chain and private key
readPemCredentials :: FilePath -> IO (Either Text (CertificateChain, PrivKey))
readPemCredentials pemPath = do
  result <- try $ do
    certs :: [X509.SignedExact X509.Certificate] <- X509File.readSignedObject pemPath
    [privKey] <- X509File.readKeyFile pemPath -- Assumes one private key in file.
    let certChain = CertificateChain certs
    pure (certChain, privKey)
  case result of
    Left (e :: SomeException) -> pure $ Left $ "Exception reading PEM file: " <> T.pack (show e)
    Right res -> pure $ Right res

instance HttpWrite App where
  postFile fp emailAddress = do
    config <- ask
    let host = serverUri config
        url = renderStr host <> "/files"

    logInfoN $ "Uploading file at " <> T.pack fp <> " to " <> T.pack url

    Authority{..} <- case uriAuthority host of
      Left _ -> throwError $ PostError "Configured server URL cannot be a relative URL."
      Right auth -> pure auth

    eCred <- liftIO $ readPemCredentials (pemPath config)

    managerSettings <- case eCred of
          Left errMsg -> do
            logErrorN $ "Failed to load client certificate: " <> errMsg
            throwError $ PostError errMsg
          Right (certChain, privKey) -> do
            let hostname = T.unpack $ unRText authHost
                hooks = def { onCertificateRequest = const (pure . Just $ (certChain, privKey))
                            , onServerCertificate = \_ _ _ _ -> pure [] -- FIXME: doing this for now as I don't know a better way to prevent the CA check
                            }
                clientParams = (defaultParamsClient hostname "") { clientHooks = hooks }
                tlsSettings = TLSSettings clientParams
                settings = TLS.mkManagerSettings tlsSettings Nothing
            pure $ settings { managerResponseTimeout = responseTimeoutMicro 60000000 }

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
            logErrorN $ ("Error uploading file." :: Text) <> show res
            throwError $ PostError "Non-success HTTP status returned"

getFileOwnerUsername :: FilePath -> IO Text
getFileOwnerUsername path = do
  status <- getFileStatus path
  let ownerId = fileOwner status
  userEntry <- getUserEntryForID ownerId
  return . T.pack . userName $ userEntry

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
