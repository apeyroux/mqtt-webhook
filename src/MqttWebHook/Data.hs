{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module MqttWebHook.Data (
  MqttClient (..)
  , MqttSubscribe (..)
  , MqttHookResponse (..)
  , Topic (..)
  ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import Data.Text as T
import GHC.Generics

data Topic = Topic {
  topicPath :: Text
  , topicQos :: Integer
} deriving (Generic, Show)

instance ToJSON Topic
instance FromJSON Topic where
  parseJSON (Object v) = Topic
        <$> v .: "topic"
        <*> v .: "qos"

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
  , msMountpoint :: Text
  , msTopics :: [Topic]
} deriving (Generic, Show)

instance ToJSON MqttSubscribe
instance FromJSON MqttSubscribe where
  parseJSON (Object v) = MqttSubscribe
        <$> v .: "username"
        <*> v .: "client_id"
        <*> v .: "mountpoint"
        <*> v .: "topics"

data MqttHookResponse = MqttHookResponseOk
  | MqttHookResponseNotAllowed
  | MqttHookResponseNext
  deriving (Generic, Show)

instance ToJSON MqttHookResponse where
  toJSON MqttHookResponseOk = Object (Data.HashMap.Strict.fromList [("result", String "ok")])
  toJSON MqttHookResponseNotAllowed = Object
        (Data.HashMap.Strict.fromList
                [ ( "result"
                  , Object
                          (Data.HashMap.Strict.fromList
                                  [("error", String "not_allowed")]
                          )
                  )
                ]
        )
  toJSON MqttHookResponseNext = Object (Data.HashMap.Strict.fromList [("result", String "next")])
