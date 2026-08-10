{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Caldron.Servant
  ( WebIDUserAuth
  , genAuthServerContext
  ) where

import           Protolude                        hiding (Handler)

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Char8       as Char8
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Data.PEM                         (PEM, pemContent, pemParseLBS)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding                    as T
import qualified Data.X509                        as X509
import           Network.HTTP.Types               (HeaderName, urlDecode)
import           Network.Wai                      (Request, requestHeaders)
import           Servant                          (Handler)
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server                   (Context (EmptyContext, (:.)),
                                                   err400, err401, errBody)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Text.URI                         (mkURI)

import Caldron.Config (Config, serviceTokens)
import Caldron.Profile (ProfileCache)
import Caldron.User (User(..))
import Caldron.WebID (WebID)
import qualified Caldron.User as User

type WebIDUserAuth = AuthProtect "webid-auth"
type instance AuthServerData WebIDUserAuth = User

validateUser :: ProfileCache -> ByteString -> Handler User
validateUser cache bs = do
  let decodedBS = urlDecode True bs
  parsePEM decodedBS >>= getCertificate >>= validateWebProfile

    where
      parsePEM :: ByteString -> Handler PEM
      parsePEM bs' = case pemParseLBS (LBS.fromStrict bs') of
        Left e -> throwError err400 { errBody = "Could not decode PEM file: " <> Char8.pack e }
        Right pems -> case pems of
          []    -> throwError err400 { errBody = "No PEM data found." }
          pem:_ -> pure pem

      getCertificate :: PEM -> Handler X509.Certificate
      getCertificate pem = case X509.decodeSignedCertificate (pemContent pem) of
        Left s -> throwError err400 { errBody = "Could not decode certificate: " <> Char8.pack s }
        Right (cert :: X509.SignedCertificate) -> pure . X509.signedObject . X509.getSigned $ cert

      getAltName :: X509.Certificate -> Handler X509.ExtSubjectAltName
      getAltName cert = do
        let es = X509.certExtensions cert
            maybeAltName = X509.extensionGet es :: Maybe X509.ExtSubjectAltName
        case maybeAltName of
          Nothing -> throwError $ err400 { errBody = "Could not parse Subject Alternative Names from certificate." }
          Just an -> pure an

      getWebIdFromAltName :: X509.ExtSubjectAltName -> Handler WebID
      getWebIdFromAltName (X509.ExtSubjectAltName ((X509.AltNameURI uriText):_)) =
        case mkURI (T.pack uriText) of
          Just uri -> pure uri
          Nothing -> throwError $ err400 { errBody = "Could not parse a URI from the given text: " <>  Char8.pack uriText }
      getWebIdFromAltName san = throwError $ err400 { errBody = "Could not read a WebID from the given subject alternative name: " <> show san }

      validateWebProfile :: X509.Certificate -> Handler User
      validateWebProfile cert = do
        webId' <- getAltName cert >>= getWebIdFromAltName
        eitherRes <- liftIO . User.run $ User.validateUser cache webId' cert
        case eitherRes of
          Left e -> throwError $ err401 { errBody = "Client certificate validation failed: " <> (LBS.fromStrict . T.encodeUtf8 . User.errorToText $ e) }
          Right user -> pure user

extractBearer :: ByteString -> Maybe T.Text
extractBearer bs = T.decodeUtf8 <$> BS.stripPrefix "Bearer " bs

resolveServiceUser :: Map.Map HeaderName ByteString -> Handler User
resolveServiceUser headers =
  case Map.lookup "X-Forwarded-Webid" headers >>= mkURI . T.decodeUtf8 of
    Just uri -> pure User { webId = uri, name = Nothing }
    Nothing  -> throwError $ err401 { errBody = "Missing X-Forwarded-Webid header" }

authHandler :: Config -> ProfileCache -> AuthHandler Request User
authHandler config cache = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    handler :: Request -> Handler User
    handler req = do
      let headers = Map.fromList (requestHeaders req)
      case Map.lookup "Authorization" headers >>= extractBearer of
        Just token ->
          if Set.member token (serviceTokens config)
            then resolveServiceUser headers
            else throw401 "Invalid service token"
        Nothing ->
          -- nginx sends X-SSL-Client-Cert; Traefik sends X-Forwarded-Tls-Client-Cert.
          case Map.lookup "X-Forwarded-Tls-Client-Cert" headers
            <|> Map.lookup "X-SSL-Client-Cert" headers of
            Nothing   -> throw401 "Missing client certificate header"
            Just cert -> validateUser cache cert

genAuthServerContext :: Config -> ProfileCache -> Context (AuthHandler Request User ': '[])
genAuthServerContext config cache = authHandler config cache :. EmptyContext
