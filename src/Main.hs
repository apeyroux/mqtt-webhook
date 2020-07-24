{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Text (Text)
import Data.Text as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.API
import Servant.Ekg
import qualified System.Remote.Monitoring as EKG
import System.Metrics


type IMEI = Text
type IdTel = Text

data Authentification = Auth | NeoToken Text | Ident IMEI IdTel | Application | Anonymous deriving Show

data MqttClient = MqttClient {
  mcUsername :: Text
  , mcPassword :: Maybe Text
  , mcId :: Text
} deriving (Generic, Show)

instance ToJSON MqttClient
instance FromJSON MqttClient where
  parseJSON (Object v) = MqttClient
        <$> v .: "username"
        <*> v .:? "password"
        <*> v .: "client_id"

data MqttSubscribe = MqttSubscribe {
  msUsername :: Text
  , msId :: Text
} deriving (Generic, Show)

instance ToJSON MqttSubscribe
instance FromJSON MqttSubscribe where
  parseJSON (Object v) = MqttSubscribe
        <$> v .: "username"
        <*> v .: "client_id"

data MqttHookResponse = MqttHookResponse {
  mhrResult :: Text
} deriving (Generic, Show)

instance ToJSON MqttHookResponse where
  toJSON (MqttHookResponse msg) = object ["result" .= msg]

data NeoTokenCheckResponse = NeoTokenCheckResponse {
  ntcrAuthentification :: Bool
  , ntcrUid :: Text
  } deriving (Generic, Show)

instance ToJSON NeoTokenCheckResponse where
  toJSON (NeoTokenCheckResponse auth uid) = object ["authentification" .= auth, "uid" .= uid]

instance FromJSON NeoTokenCheckResponse where
  parseJSON (Object v) = NeoTokenCheckResponse <$> v .: "authentification" <*> v .: "uid"

type MqttWebHook = "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttClient :> Post '[JSON] MqttHookResponse
  :<|> "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttSubscribe :> Post '[JSON] MqttHookResponse
type NeoToken = "token" :> "check" :> Capture "uid" Text :> Capture "token" Text :> Get '[JSON] NeoTokenCheckResponse

mc2auth :: MqttClient -> Authentification
mc2auth c = case splitOn ":" (mcUsername c) of
  -- kk mais ok pour poc
  ["neoparc"] -> case mcPassword c of
    Just "neoparc" -> Application
    _ ->  Anonymous
  ["token", t, _] -> NeoToken t
  ["ident", imei, idtel] -> Ident imei idtel
  ["auth", _, _] -> Auth
  _ -> Anonymous

mqttWebHook :: Proxy MqttWebHook
mqttWebHook = Proxy

neoToken :: Proxy NeoToken
neoToken = Proxy

neoTokenAPI :: Text -> Text -> Handler NeoTokenCheckResponse
neoTokenAPI uid token = return NeoTokenCheckResponse { ntcrAuthentification = True, ntcrUid = "untoken" }

whAuthOnSubscribe :: Maybe Text -> MqttSubscribe -> Handler MqttHookResponse
whAuthOnSubscribe (Just "auth_on_subscriber") _ = return $ MqttHookResponse "on_sub"
whAuthOnSubscribe _ _ = return $ MqttHookResponse "on_sub_bad"

whAuthOnRegister :: Maybe Text -> MqttClient -> Handler MqttHookResponse
whAuthOnRegister (Just "auth_on_register") c = do
  case mc2auth c of
    NeoToken t -> do
      liftIO $ do
        print $ "check token:" <> t
        print c
      return $ MqttHookResponse "ok"
    Application -> do
      liftIO $ print "hello application"
      return $ MqttHookResponse "ok"
    Ident imei _ -> do
      liftIO $ do
        print $ "IDENT on_regiserer de " <> imei
        print c
      return $ MqttHookResponse "ok"
    Auth -> do
      liftIO $ do
        print "AUTH on_regiserer"
        print "TODO: Mettre dans un redis une clef login avec value neotoken et une ttl. si ttl alors 401"
        print c
      return $ MqttHookResponse "ok"
    _ -> do
      liftIO $ do
        print "NO AUTH"
        print c
      return $ MqttHookResponse "next"
whAuthOnRegister _ _ = return $ MqttHookResponse "next"

srvMqttWebHook :: Server MqttWebHook
srvMqttWebHook = whAuthOnRegister :<|> whAuthOnSubscribe

srvNeoToken :: Server NeoToken
srvNeoToken = neoTokenAPI

appNeoToken :: IO Application
appNeoToken = do
  store <- EKG.serverMetricStore <$> EKG.forkServer "0.0.0.0" 8889
  monitorEndpoints' <- monitorEndpoints neoToken store
  return $ monitorEndpoints' (serve neoToken srvNeoToken)

appMqttWebHook :: IO Application
appMqttWebHook = do
  store <- EKG.serverMetricStore <$> EKG.forkServer "0.0.0.0" 8888
  monitorEndpoints' <- monitorEndpoints mqttWebHook store
  return $ monitorEndpoints' (serve mqttWebHook srvMqttWebHook)

main :: IO ()
main = do
  putStrLn "starting mqtt hook listener ..."
  withStdoutLogger $ \aplogger -> do
    -- let settings = setPort 8080 $ setLogger aplogger defaultSettings
    -- appWH <- appMqttWebHook
    -- runSettings settings appWH

    -- neotoken
    let settings = setPort 8081 $ setLogger aplogger defaultSettings
    appNT <- appNeoToken
    runSettings settings appNT
