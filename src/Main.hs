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

data Authentification = Auth | Ident | Application | Anonymous deriving Show

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

data TokenCheckResponse = TokenCheckResponse {
  tcrAuthentification :: Bool
  , tcrUid :: Text
  } deriving (Generic, Show)

instance ToJSON TokenCheckResponse where
  toJSON (TokenCheckResponse auth uid) = object ["authentification" .= auth, "uid" .= uid]

instance FromJSON TokenCheckResponse where
  parseJSON (Object v) = TokenCheckResponse <$> v .: "authentification" <*> v .: "uid"

type MqttWebHook = "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttClient :> Post '[JSON] MqttHookResponse
type NeoTokenAPI = "token" :> "check" :> Capture "uid" :> Capture "token" :> Get '[JSON] TokenCheckResponse

mc2auth :: MqttClient -> Authentification
mc2auth c = case splitOn ":" (mcUsername c) of
  -- kk mais ok pour poc
  ["neoparc"] -> case mcPassword c of
    Just "neoparc" -> Application
    _ ->  Anonymous
  ["ident", _, _] -> Ident
  ["auth", _, _] -> Auth
  _ -> Anonymous

mqttWebHook :: Proxy MqttWebHook
mqttWebHook = Proxy

whrOk :: Maybe Text -> MqttClient -> Handler MqttHookResponse
whrOk (Just "auth_on_register") c = do
  case mc2auth c of
    Application -> do
      liftIO $ print "hello application"
      return $ MqttHookResponse "ok"
    Ident -> do
      liftIO $ do
        print "IDENT on_regiserer"
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

appMqttWebHook :: Application
appMqttWebHook = serve mqttWebHook srvMqttWebHook

main :: IO ()
main = do
  putStrLn "starting mqtt hook listener ..."
  withStdoutLogger $ \aplogger -> do
        let settings = setPort 80 $ setLogger aplogger defaultSettings
        runSettings settings appMqttWebHook
