{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module MqttWebHook.Data (
  MqttClient (..)
  , MqttSubscribe (..)
  , MqttHookResponse (..)
  ) where

import           Data.Aeson
import           Data.Text (Text)
import           Data.Text as T
import           GHC.Generics

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

data MqttHookResponse = MqttHookResponseOk {
  mhrResultOk :: Text
  }
  | MqttHookResponseNotAllowed {
    mhrResultKo :: Text
  } deriving (Generic, Show)

instance ToJSON MqttHookResponse where
  toJSON (MqttHookResponseOk msg) = object ["result" .= msg]
  toJSON (MqttHookResponseNotAllowed msg) = object ["result" .= msg]
