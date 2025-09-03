{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RainbowHash.Profile
  ( getProfileData
  , RDF4HError(..)
  ) where

import           Protolude

import           Control.Monad.Logger (MonadLogger, logDebugN, logErrorN,
                                       logInfoN)
import           Data.RDF             (RDF, Rdf, TurtleParser (..))
import qualified Data.RDF             as RDF
import qualified Data.Text            as T
import           Text.URI             (render)

import           RainbowHash.Crypto   (CertificateData (..), ProfileData (..))
import           RainbowHash.User     (WebID)

data RDF4HError = RDFParseError RDF.ParseFailure
                | CertificateError Text
                deriving (Show)

getProfileGraph
  :: ( Rdf a
     , MonadError RDF4HError m
     , MonadLogger m
     , MonadIO m
     )
  => WebID
  -> m (RDF a)
getProfileGraph webId' = do
  logInfoN $ "Fetching profile document from " <> render webId'
  let url = toS . render $ webId'
      turtleParser :: TurtleParser
      turtleParser = TurtleParser Nothing Nothing
  eitherGraph <- liftIO $ RDF.parseURL turtleParser url
  case eitherGraph of
    Left err    -> throwError $ RDFParseError err
    Right graph -> pure graph

parseProfileData
  :: forall m a.
  ( MonadError RDF4HError m
  , MonadLogger m
  , Rdf a
  )
  => RDF a
  -> m ProfileData
parseProfileData g = do
  logInfoN "Fetching name from user profile document."

  name <- queryName g
  certData <- queryCertData g

  pure ProfileData {..}

queryCertData
  :: ( MonadError RDF4HError m
     , MonadLogger m
     , Rdf a
     )
  => RDF a
  -> m (NonEmpty CertificateData)
queryCertData g = do
  logInfoN "Querying certificate data from user profile document."
  logDebugN $ "Profile graph: " <> T.pack (RDF.showGraph g)
  let triples = RDF.query g Nothing (Just (RDF.UNode "http://www.w3.org/ns/auth/cert#key")) Nothing
      certNodes = object <$> triples
      eitherCertData = getCertData g <$> certNodes
      (errs, certData') = partitionEithers eitherCertData

  logDebugN $ "Number of Certificate nodes found: " <> show (length certNodes)
  logDebugN $ "Certificate nodes found: " <> show certNodes

  -- log errors
  forM_ errs logErrorN

  case nonEmpty certData' of
    Nothing -> throwError $ CertificateError "Could not read certificate data from profile document."
    Just certData'' -> pure certData''

object :: RDF.Triple -> RDF.Subject
object (RDF.Triple _ _ o) = o

getCertData
  :: ( MonadError Text m
     , Rdf a
     )
  => RDF a
  -> RDF.Node
  -> m CertificateData
getCertData g' n = CertificateData <$> getModulus g' n <*> getExponent g' n

getModulus
  :: ( MonadError Text m
     , Rdf a
     )
  => RDF a
  -> RDF.Node
  -> m Integer
getModulus g n = do
  let modTriples = RDF.query g (Just n) (Just (RDF.UNode "http://www.w3.org/ns/auth/cert#modulus")) Nothing
  case modTriples of
    [] -> throwError "Could not retrieve certificate modulus from profile data."
    -- FIXME: consider all literal nodes, not just the typed version.
    [RDF.Triple _ _ (RDF.LNode (RDF.TypedL modHexStr _))] ->
      case T.hexadecimal modHexStr of
        Left s -> throwError $ "Could not read certificate modulus from profile data: " <> T.pack s
        Right (i, _) -> pure i
    triples -> throwError $ "Could not read certificate modulus from profile data: " <> show triples

getExponent
  :: ( MonadError Text m
     , Rdf a
     )
  => RDF a
  -> RDF.Node
  -> m Integer
getExponent g n = do
  let expTriples = RDF.query g (Just n) (Just (RDF.UNode "http://www.w3.org/ns/auth/cert#exponent")) Nothing
  case expTriples of
    [] -> throwError "Could not retrieve certificate exponent from profile data."
    -- FIXME: consider all literal nodes, not just the typed version.
    [RDF.Triple _ _ (RDF.LNode (RDF.TypedL expStr _))] ->
      case readMaybe expStr of
        Nothing -> throwError "Could not read certificate exponent from profile data."
        Just i -> pure i
    triples -> throwError $ "Could not read certificate exponent from profile data: " <> show triples

queryName
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> m (Maybe Text)
queryName g' = runMaybeT $ queryFirstNameFOAF g' <|> queryFirstNameSchema g' <|> queryNameFOAF g'

queryFirstNameFOAF
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> MaybeT m Text
queryFirstNameFOAF g' = do
  let triples = RDF.query g' Nothing (Just (RDF.UNode "http://xmlns.com/foaf/0.1/givenName")) Nothing
  case triples of
    [] -> empty
    -- FIXME: look at all triples, not just the first
    triple:_ -> case triple of
      -- FIXME: consider all literal nodes, not just the plain version.
      RDF.Triple _ _ (RDF.LNode (RDF.PlainL firstName)) -> pure firstName
      _ -> empty

queryFirstNameSchema
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> MaybeT m Text
queryFirstNameSchema g' = do
  let triples = RDF.query g' Nothing (Just (RDF.UNode "https://schema.org/givenName")) Nothing
  case triples of
    [] -> empty
    -- FIXME: look at all triples, not just the first
    triple:_ -> case triple of
      -- FIXME: consider all literal nodes, not just the plain version.
      RDF.Triple _ _ (RDF.LNode (RDF.PlainL firstName)) -> pure firstName
      _ -> empty

queryNameFOAF
  :: ( MonadLogger m
     , Rdf a
     )
  => RDF a
  -> MaybeT m Text
queryNameFOAF g' = do
  let triples = RDF.query g' Nothing (Just (RDF.UNode "http://xmlns.com/foaf/0.1/name")) Nothing
  case triples of
    [] -> empty
    -- FIXME: look at all triples, not just the first
    triple:_ -> case triple of
      -- FIXME: consider all literal nodes, not just the plain version.
      RDF.Triple _ _ (RDF.LNode (RDF.PlainL firstName)) -> pure firstName
      _ -> empty

getProfileData
  :: ( MonadError RDF4HError m
     , MonadLogger m
     , MonadIO m
     )
  => WebID
  -> m ProfileData
getProfileData webId' =
  getProfileGraph @RDF.TList webId' >>= parseProfileData
