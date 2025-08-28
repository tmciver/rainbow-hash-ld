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
  ) where

import Protolude hiding (exponent)

import qualified Crypto.PubKey.RSA as Crypto
import qualified Data.X509                             as X509

import           Control.Monad.Error                   (mapError)
import           RainbowHash.User (WebID, User(..))
import qualified RainbowHash.HTTPClient as HTTP

newtype CryptoApp a = CryptoApp { getExceptT :: ExceptT CryptoError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CryptoError)

run :: CryptoApp a -> IO (Either CryptoError a)
run = runExceptT . getExceptT

data CryptoError
  = Unauthorized Text
  | HTTPError HTTP.HTTPClientError

errorToText :: CryptoError -> Text
errorToText (Unauthorized t) = "Unauthorized: " <> t
errorToText (HTTPError hce) = HTTP.httpClientErrorToString hce

getPublicKey
  :: MonadError CryptoError m
  => X509.Certificate
  -> m Crypto.PublicKey
getPublicKey cert = undefined

validateCert
  :: MonadError CryptoError m
  => X509.Certificate
  -> Integer
  -> Integer
  -> m ()
validateCert cert mod' exp' = do
  Crypto.PublicKey {..} <- getPublicKey cert
  unless (mod' == public_n && exp' == public_e) $
    throwError $ Unauthorized "Certificate did not validate against profile data."

validateUser
  :: ( MonadError CryptoError m
     , MonadIO m
     )
  => WebID
  -> X509.Certificate
  -> m User
validateUser webId' cert = do
  -- 1. fetch user's profile document
  HTTP.ProfileData{..} <- mapError HTTPError (HTTP.getProfileData webId')

  -- 2. compare the modulus and exponent to that in the certificate
  validateCert cert modulus exponent

  -- 3. if they validate, return user.
  pure $ User webId' firstName lastName
