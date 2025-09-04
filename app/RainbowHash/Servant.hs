{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module RainbowHash.Servant
  ( WebID
  , WebIDUserAuth
  , genAuthServerContext
  ) where

import           Protolude                        hiding (Handler)

import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Char8       as Char8
import qualified Data.Map                         as Map
import           Data.PEM                         (PEM, pemContent, pemParseLBS)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding                    as T
import qualified Data.X509                        as X509
import           Network.HTTP.Types               (urlDecode)
import           Network.Wai                      (Request, requestHeaders)
import           Servant                          (Handler)
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server                   (Context (EmptyContext, (:.)),
                                                   err400, err401, errBody)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Text.URI                         (mkURI)

import RainbowHash.User (User)
import RainbowHash.WebID (WebID)
import qualified RainbowHash.User as User

type WebIDUserAuth = AuthProtect "webid-auth"
type instance AuthServerData WebIDUserAuth = User

validateUser :: ByteString -> Handler User
validateUser bs = do
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
        eitherRes <- liftIO . User.run $ User.validateUser webId' cert
        case eitherRes of
          Left e -> throwError $ err401 { errBody = "Client certificate validation failed: " <> (LBS.fromStrict . T.encodeUtf8 . User.errorToText $ e) }
          Right user -> pure user

--- | The auth handler wraps a function from Request -> Handler WebID.
--- We look for the client certificate in the X-SSL-CERT request header.
--- The client certificate text is then passed to our `getWebid` function.
authHandler :: AuthHandler Request User
authHandler = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    handler :: Request -> Handler User
    handler req =
      req & requestHeaders
          & Map.fromList
          & Map.lookup "X-SSL-CERT"
          & maybeToEither "Missing X-SSL-CERT header"
          & either throw401 validateUser

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Context (AuthHandler Request User ': '[])
genAuthServerContext = authHandler :. EmptyContext
