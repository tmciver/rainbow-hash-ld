{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module RainbowHash.Crypto
  ( validateUser
  , CryptoError(..)
  , CryptoApp
  , run
  , errorToText
  , ProfileData(..)
  , CertificateData(..)
  ) where

import Protolude hiding (exponent)

import Control.Monad.Logger (MonadLogger (..), fromLogStr, toLogStr, logInfoN)
import qualified Crypto.PubKey.RSA as Crypto
import qualified Data.X509                             as X509

import           Control.Monad.Error                   (mapError)
import           RainbowHash.User (WebID, User(..))
import qualified RainbowHash.HTTPClient as HTTP
import           RainbowHash.Logger            (writeLog)

newtype CryptoApp a = CryptoApp { getExceptT :: ExceptT CryptoError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CryptoError)

instance MonadLogger CryptoApp where
  monadLoggerLog _ _ logLevel = liftIO . writeLog logLevel . decodeUtf8 . fromLogStr . toLogStr

run :: CryptoApp a -> IO (Either CryptoError a)
run = runExceptT . getExceptT

data CryptoError
  = Unauthorized Text
  | CertificateError Text
  | HTTPError HTTP.HTTPClientError

errorToText :: CryptoError -> Text
errorToText (Unauthorized t) = "Unauthorized: " <> t
errorToText (CertificateError t) = "Certificate error: " <> t
errorToText (HTTPError hce) = HTTP.httpClientErrorToString hce

getPublicKey
  :: MonadError CryptoError m
  => X509.Certificate
  -> m Crypto.PublicKey
getPublicKey cert =
  case X509.certPubKey cert of
    X509.PubKeyRSA pubkey -> pure pubkey
    _ -> throwError $ CertificateError "Non-RSA public key found. Only RSA public supported at this time."

validateCert
  :: ( MonadError CryptoError m
     , MonadLogger m
     )
  => X509.Certificate
  -> NonEmpty CertificateData
  -> m ()
validateCert cert certDataList = do
  publicKey <- getPublicKey cert
  let validated = getAny $ foldMap (Any . isValidCert publicKey) certDataList
  if validated
    then logInfoN "Certificate has been validated."
    else do
      let msg = "Certificate did not validate against profile data."
      logInfoN msg
      throwError $ Unauthorized msg
  where isValidCert :: Crypto.PublicKey -> CertificateData -> Bool
        isValidCert Crypto.PublicKey{..} CertificateData{..} =
          cdModulus == public_n && cdExponent == public_e

validateUser
  :: ( MonadError CryptoError m
     , MonadLogger m
     , MonadIO m
     )
  => WebID
  -> X509.Certificate
  -> m User
validateUser webId' cert = do
  -- 1. fetch user's profile document
  ProfileData{..} <- mapError HTTPError (HTTP.getProfileData webId')

  -- 2. compare the modulus and exponent to that in the certificate
  validateCert cert certData

  -- 3. if they validate, return user.
  pure $ User webId' name

data CertificateData = CertificateData
  { cdModulus :: Integer
  , cdExponent :: Integer
  }

data ProfileData = ProfileData
  { certData :: NonEmpty CertificateData
  , name :: Maybe Text
  }
