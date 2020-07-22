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
type NeoTokenAPI = "token" :> "check" :> Capture "uid" :> Capture "token" :> Get '[JSON] NeoTokenCheckResponse

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

whrOk :: Maybe Text -> MqttClient -> Handler MqttHookResponse
whrOk (Just "auth_on_register") c = do
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
-- whrOk (Just "auth_on_subscribe") c = do
--   liftIO $ print "on_subscribe"
--   liftIO $ print c
--   return $ MqttHookResponse "ok"
whrOk _ _ = return $ MqttHookResponse "next"

srvMqttWebHook :: Server MqttWebHook
srvMqttWebHook = whrOk

appMqttWebHook :: IO Application
appMqttWebHook = do
  store <- EKG.serverMetricStore <$> EKG.forkServer "127.0.0.1" 8888
  monitorEndpoints' <- monitorEndpoints mqttWebHook store
  return $ monitorEndpoints' (serve mqttWebHook srvMqttWebHook)

main :: IO ()
main = do
  putStrLn "starting mqtt hook listener ..."
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    app <- appMqttWebHook
    runSettings settings app
