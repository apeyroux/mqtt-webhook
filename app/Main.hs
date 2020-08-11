{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad.Trans (liftIO)
import           Data.Aeson
import           Data.Text (Text)
import           Data.Text as T
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger (withStdoutLogger)
import           Servant
import           Servant.API
import           Servant.Ekg
import qualified System.Remote.Monitoring as EKG
import           System.Metrics

import           Data.Proxy
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.Client

import           Servant.PY
import           System.FilePath

import           MqttWebHook.Data

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

-- NEOTOKEN
data NeoTokenV1 =
  NeoTokenIsAuth {
    ntcrAuthentification :: Bool
    , ntcrUid :: Text
  }
  | NeoTokenIsUnAuth {
    ntcrError :: Int
    , ntcrMsg :: Text
  } deriving (Generic, Show)

instance FromJSON NeoTokenV1 where
  parseJSON (Object v) = NeoTokenIsAuth <$> v .: "authentification" <*> v .: "uid"

type NeoTokenV1API = "token" :> "check" :> QueryParam "uid" Text :> QueryParam "token" Text :> Get '[JSON] NeoTokenV1

neoTokenV1API :: Proxy NeoTokenV1API
neoTokenV1API = Proxy

-- query ?format=json
clientNeotoken :: Maybe Text -> Maybe Text -> ClientM NeoTokenV1
clientNeotoken = client neoTokenV1API
-- END NEOTOKEN

type IMEI = Text
type IdTel = Text
type TUid = Text
type TContent = Text

data Authentification = Auth | NeoToken IMEI IdTel TUid TContent  | Ident IMEI IdTel | Application | Anonymous deriving Show

type MqttWebHook = "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttClient :> Post '[JSON] MqttHookResponse
  :<|> "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttSubscribe :> Post '[JSON] MqttHookResponse

mc2auth :: MqttClient -> Authentification
mc2auth c = case splitOn ":" (mcUsername c) of
  ["neoparc"] -> case mcPassword c of
    Just "neoparc" -> Application
    _ ->  Anonymous
  ["token", imei, idtel, uid, token] -> NeoToken imei idtel uid token
  ["ident", imei, idtel] -> Ident imei idtel
  ["auth", _, _] -> Auth
  _ -> Anonymous

mqttWebHookAPI :: Proxy MqttWebHook
mqttWebHookAPI = Proxy

whAuthOnSubscribe :: Maybe Text -> MqttSubscribe -> Handler MqttHookResponse
whAuthOnSubscribe (Just "auth_on_subscriber") _ = return $ MqttHookResponse "on_sub"
whAuthOnSubscribe _ _ = return $ MqttHookResponse "on_sub_bad"

whAuthOnRegister :: Maybe Text -> MqttClient -> Handler MqttHookResponse
whAuthOnRegister (Just "auth_on_register") c = 
  case mc2auth c of
    NeoToken imei idtel uid token ->
      -- Check d'un NeoToken
      liftIO $ do
        print $ "IDENT d'un token " <> token
        managerNeoToken <- newManager defaultManagerSettings
        r <- runClientM (clientNeotoken (Just uid) (Just token)) (mkClientEnv managerNeoToken (BaseUrl Http "127.0.0.1" 8081 ""))
        case r of
          Left e -> return $ MqttHookResponseNotAllowed $ T.pack $ show e
          Right t -> return $ MqttHookResponseOk $ T.pack $ show t
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
      throwError err401
      -- return $ MqttHookResponse "next"
whAuthOnRegister _ _ = return $ MqttHookResponse "next"

srvMqttWebHook :: Server MqttWebHook
srvMqttWebHook = whAuthOnRegister :<|> whAuthOnSubscribe

appMqttWebHook :: IO Application
appMqttWebHook = do
  monitorEndpoints' <- monitorEndpoints mqttWebHookAPI =<< (EKG.serverMetricStore <$> EKG.forkServer "0.0.0.0" 8000)
  return $ monitorEndpoints' (serve mqttWebHookAPI srvMqttWebHook)
  -- return $ serve mqttWebHookAPI srvMqttWebHook

main :: IO ()
main = do
  putStrLn "write python sample ..."
  writePythonForAPI mqttWebHookAPI requests (result </> "api.py")
  putStrLn "starting mqtt hook listener ..."
  withStdoutLogger $ \appLogger -> do
    let settings = setHost "0.0.0.0" $ setPort 8080 $ setLogger appLogger defaultSettings
    appMqttWebHook >>= runSettings settings
  where
    result :: FilePath
    result = "samples"
