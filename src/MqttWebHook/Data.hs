{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module MqttWebHook.Data (
  MqttHookQuery (..)
  , MqttHookResponse (..)
  , Topic (..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import Data.Text as T
import GHC.Generics

data MqttHookQuery = MqttClient {
  mcUsername :: Text
  , mcPassword :: Maybe Text
  , mcId :: Text
} | MqttSubscribe {
  msUsername :: Text
  , msId :: Text
  , msMountpoint :: Text
  , msTopics :: [Topic]
} | MqttPublish {
  mpUsername :: Text
  , mpId :: Text
  , mpMountpoint :: Text
  , mpQos :: Int
  , mpTopic :: Text
  , mpPayload :: Text
  , mpRetain :: Bool
} deriving (Generic, Show)

instance ToJSON MqttHookQuery
instance FromJSON MqttHookQuery where
  parseJSON (Object v) = parsePublish <|> parseSub <|> parseClient
    where
      parseSub = MqttSubscribe
        <$> v .: "username"
        <*> v .: "client_id"
        <*> v .: "mountpoint"
        <*> v .: "topics"
      parseClient = MqttClient
        <$> v .: "username"
        <*> v .:? "password"
        <*> v .: "client_id"
      parsePublish = MqttPublish
        <$> v .: "username"
        <*> v .: "client_id"
        <*> v .: "mountpoint"
        <*> v .: "qos"
        <*> v .: "topic"
        <*> v .: "payload"
        <*> v .: "retain"

data Topic = Topic {
  topicPath :: Text
  , topicQos :: Integer
} deriving (Generic, Show)

instance ToJSON Topic
instance FromJSON Topic where
  parseJSON (Object v) = Topic
        <$> v .: "topic"
        <*> v .: "qos"

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
