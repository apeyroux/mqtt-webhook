{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.API

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

type MqttWebHook = "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttClient :> Post '[JSON] MqttHookResponse

mqttWebHook :: Proxy MqttWebHook
mqttWebHook = Proxy

whrOk :: Maybe Text -> MqttClient -> Handler MqttHookResponse
whrOk (Just "auth_on_register") c = do
  liftIO $ print "on_regiserer"
  liftIO $ print c
  return $ MqttHookResponse "ok"
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
