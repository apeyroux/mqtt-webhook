{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad.Trans (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import           Data.Text as T
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger (withStdoutLogger)
import           Servant
import           Servant.API
import           Servant.Ekg
import           System.Environment
import           System.Exit

import qualified System.Remote.Monitoring as EKG
import           System.Metrics

import           Data.Proxy
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.Client

import           Servant.PY
import           System.FilePath

import           Control.Monad.Trans.Reader

import           MqttWebHook.Data


-- CONFIG
data Configuration = Configuration {
  cfgListenPort :: Int
  , cfgEkgListenPort :: Int
  , cfgNeoTokenBaseURL :: Text
  , cfgNeoTokenPort :: Int
  , cfgUsers :: [User]
} deriving (Generic, Show)
data User = User {
  uLogin :: Text
  , uPassword :: Text
  , uCanWip :: Bool
} deriving (Generic, Show)

instance FromJSON Configuration
instance FromJSON User

instance ToJSON Configuration
instance ToJSON User
-- ENF CONFIG

-- CLIENT NEOTOKEN
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

clientNeotoken :: Maybe Text -> Maybe Text -> ClientM NeoTokenV1
clientNeotoken = client neoTokenV1API
-- END CLIENT NEOTOKEN

type IMEI = Text
type IdTel = Text
type TUid = Text
type TContent = Text

data Authentification = Auth | NeoToken IMEI IdTel TUid TContent  | Ident IMEI IdTel | Application | Anonymous deriving Show

type MqttWebHook = "auth" :> Header "vernemq-hook" Text :> ReqBody '[JSON] MqttHookQuery :> Post '[JSON] MqttHookResponse

mc2auth :: MqttHookQuery -> Authentification
mc2auth c@(MqttClient uname _ _) = case splitOn ":" uname of
  ["neoparc"] -> case mcPassword c of
    Just "neoparc" -> Application
    _ ->  Anonymous
  ["token", imei, idtel, uid, token] -> NeoToken imei idtel uid token
  ["ident", imei, idtel] -> Ident imei idtel
  ["auth", _, _] -> Auth
  _ -> Anonymous

mqttWebHookAPI :: Proxy MqttWebHook
mqttWebHookAPI = Proxy

wh :: Maybe Text -> MqttHookQuery -> Handler MqttHookResponse
wh (Just "auth_on_register") c@(MqttClient uname _ _) =
  case mc2auth c of
    NeoToken imei idtel uid token ->
      -- Check d'un NeoToken
      liftIO $ do
      print $ "IDENT d'un token " <> token
      managerNeoToken <- newManager defaultManagerSettings
      r <- runClientM (clientNeotoken (Just uid) (Just token)) (mkClientEnv managerNeoToken (BaseUrl Http "127.0.0.1" 8081 ""))
      case r of
        Left e -> return MqttHookResponseNotAllowed
        Right t -> return MqttHookResponseOk
    Application -> do
      liftIO $ print "hello application"
      return MqttHookResponseOk
    Ident imei _ -> do
      -- EXEMPLE POUR ALLER CHERCHER UN WS EXT (NeoToken)
      liftIO $ print $ "IDENT on_regiserer de " <> imei
      return MqttHookResponseOk
    Auth -> do
      liftIO $ do
        print "AUTH on_regiserer"
        print "TODO: Mettre dans un redis une clef login avec value neotoken et une ttl. si ttl alors 401"
        print "ok"
      return MqttHookResponseOk
    Anonymous -> do
      liftIO $
        putStrLn $ "ANONYMOUS ! (" <> T.unpack uname <> ")"
      throwError err401
wh (Just "auth_on_subscribe") s@(MqttSubscribe user uid mnt topics) = do
  liftIO $ putStrLn $ "Subscribe de " <> T.unpack user <> " a " <> show topics
  return MqttHookResponseOk
wh (Just "auth_on_publish") p@(MqttPublish user uid _ _ topic payload _) = do
  liftIO $ do
    putStrLn $ "Publish de " <> T.unpack user <> " dans " <> T.unpack topic
    print p
  return MqttHookResponseOk
wh h q = do
  liftIO $ do
    print h
    print q
  return MqttHookResponseNext

srvMqttWebHook :: Server MqttWebHook
srvMqttWebHook = wh

appMqttWebHook :: ReaderT Configuration IO Application
appMqttWebHook = do
  cfg <- ask
  monitorEndpoints' <- liftIO $ monitorEndpoints mqttWebHookAPI =<< (EKG.serverMetricStore <$> EKG.forkServer "0.0.0.0" (cfgEkgListenPort cfg))
  return $ monitorEndpoints' (serve mqttWebHookAPI srvMqttWebHook)
  -- return $ serve mqttWebHookAPI srvMqttWebHook


testReader :: ReaderT Configuration IO ()
testReader = do
  cfg <- ask
  liftIO $ print cfg

main :: IO ()
main = do

  args <- getArgs

  case args of
    [cfgFile] -> do
      cfgraw <- B.readFile cfgFile
      case decode cfgraw :: Maybe Configuration of
        Just c -> do
          print c
          runReaderT testReader c
          putStrLn "write python sample ..."
          writePythonForAPI mqttWebHookAPI requests (result </> "api.py")
          putStrLn "starting mqtt hook listener ..."
          withStdoutLogger $ \appLogger -> do
            let settings = setHost "0.0.0.0" $ setPort 8080 $ setLogger appLogger defaultSettings
            runReaderT appMqttWebHook c >>= runSettings settings
        Nothing -> do
          putStrLn "Je ne comprend pas le fichier de configuration."
          exitFailure
    _ -> putStrLn "usage: mqtt-webhook ./path/to/config.json"

  where
    result :: FilePath
    result = "samples"
