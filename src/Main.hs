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

import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client

--- TEST IP
newtype Ip = Ip {
  ip :: String
} deriving (Eq, Show, Generic)
instance FromJSON Ip
instance ToJSON Ip

type IPifyAPI = QueryParam "format" String :> Get '[JSON] Ip

ipifyAPI :: Proxy IPifyAPI
ipifyAPI = Proxy

-- query ?format=json
ipq :: ClientM Ip
ipq = client ipifyAPI (Just "json")
-- END TEST

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

newtype MqttHookResponse = MqttHookResponse {
  mhrResult :: Text
} deriving (Generic, Show)

instance ToJSON MqttHookResponse where
  toJSON (MqttHookResponse msg) = object ["result" .= msg]

type MqttWebHook = "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttClient :> Post '[JSON] MqttHookResponse
  :<|> "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttSubscribe :> Post '[JSON] MqttHookResponse

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

whAuthOnSubscribe :: Maybe Text -> MqttSubscribe -> Handler MqttHookResponse
whAuthOnSubscribe (Just "auth_on_subscriber") _ = return $ MqttHookResponse "on_sub"
whAuthOnSubscribe _ _ = return $ MqttHookResponse "on_sub_bad"

whAuthOnRegister :: Maybe Text -> MqttClient -> Handler MqttHookResponse
whAuthOnRegister (Just "auth_on_register") c = 
  case mc2auth c of
    NeoToken t -> do
      liftIO $ do
        print $ "check token:" <> t
        print c
      return $ MqttHookResponse "ok"
    Application -> do
      liftIO $ print "hello application"
      return $ MqttHookResponse "ok"
    Ident imei _ -> 
      -- EXEMPLE POUR ALLER CHERCHER UN WS EXT (NeoToken)
      liftIO $ do
        print $ "IDENT on_regiserer de " <> imei
        manager' <- newManager defaultManagerSettings
        r <- runClientM ipq (mkClientEnv manager' (BaseUrl Http "api.ipify.org" 80 ""))
        case r of
          Left e -> return $ MqttHookResponse (T.pack $ show e)
          Right (Ip ip) -> return $ MqttHookResponse (T.pack ip)
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

appMqttWebHook :: IO Application
appMqttWebHook = do
  store <- EKG.serverMetricStore <$> EKG.forkServer "0.0.0.0" 8888
  monitorEndpoints' <- monitorEndpoints mqttWebHook store
  return $ monitorEndpoints' (serve mqttWebHook srvMqttWebHook)

main :: IO ()
main = do
  putStrLn "starting mqtt hook listener ..."
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    appWH <- appMqttWebHook
    runSettings settings appWH

    -- neotoken
    -- let settings = setPort 8081 $ setLogger aplogger defaultSettings
    -- appNT <- appNeoToken
    -- runSettings settings appNT
