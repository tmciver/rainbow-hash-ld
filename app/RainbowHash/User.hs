{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RainbowHash.User
  ( User
  , UserError(..)
  , userWebId
  , userName
  , validateUser
  , errorToText
  , UserApp
  , run
  ) where

import           Protolude

import Control.Monad.Logger (MonadLogger, monadLoggerLog, fromLogStr, toLogStr)
import qualified Data.X509                             as X509

import Control.Monad.Error (mapError)
import RainbowHash.WebID (WebID)
import RainbowHash.Profile (getProfile, Profile(..), ProfileError)
import qualified RainbowHash.Profile as Profile
import RainbowHash.Crypto (validateCert, CryptoError)
import qualified RainbowHash.Crypto as Crypto
import           RainbowHash.Logger            (writeLog)

data User = User
  { webId     :: WebID
  , name :: Maybe Text
  }

userWebId :: User -> WebID
userWebId = webId

userName :: User -> Maybe Text
userName user = user.name

data UserError
  = ProfileError ProfileError
  | CryptoError CryptoError
  | Unauthenticated

errorToText :: UserError -> Text
errorToText ue = "User error: " <> case ue of
  ProfileError pe -> Profile.errorToText pe
  CryptoError ce -> Crypto.errorToText ce
  Unauthenticated -> "unauthenticated"

newtype UserApp a = UserApp { getExceptT :: ExceptT UserError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError UserError)

instance MonadLogger UserApp where
  monadLoggerLog _ _ logLevel = liftIO . writeLog logLevel . decodeUtf8 . fromLogStr . toLogStr

run :: UserApp a -> IO (Either UserError a)
run = runExceptT . getExceptT

validateUser
  :: ( MonadError UserError m
     , MonadLogger m
     , MonadIO m
     )
  => WebID
  -> X509.Certificate
  -> m User
validateUser webId' cert = do
  -- fetch user's profile document
  Profile{..} <- mapError ProfileError (getProfile webId')

  -- compare the modulus and exponent to that in the certificate
  mapError (const Unauthenticated) (validateCert cert certData)

  -- if they validate, return user.
  pure $ User webId' name
