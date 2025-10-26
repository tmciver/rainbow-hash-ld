{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module RainbowHash.Crypto
  ( CryptoError(..)
  , CryptoApp
  , run
  , errorToText
  , CertificateData(..)
  , validateCert
  ) where

import Protolude hiding (exponent)

import Control.Monad.Logger (MonadLogger (..), fromLogStr, toLogStr, logInfoN)
import qualified Crypto.PubKey.RSA as Crypto
import qualified Data.X509                             as X509

import qualified RainbowHash.HTTPClient as HTTP
import           RainbowHash.Logger            (writeLog)

newtype CryptoApp a = CryptoApp { getExceptT :: ExceptT CryptoError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CryptoError)

instance MonadLogger CryptoApp where
  monadLoggerLog _ _ logLevel = liftIO . writeLog logLevel . decodeUtf8 . fromLogStr . toLogStr

run :: CryptoApp a -> IO (Either CryptoError a)
run = runExceptT . getExceptT

data CertificateData = CertificateData
  { cdModulus :: Integer
  , cdExponent :: Integer
  }

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
    _ -> throwError $ CertificateError "Non-RSA public key found. Only RSA public key supported at this time."

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
