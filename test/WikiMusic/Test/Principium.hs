{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WikiMusic.Test.Principium where

import Control.Concurrent
import Data.ByteString.Lazy qualified as BL
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Time
import Data.UUID (UUID)
import Data.UUID.V4
import Database.SQLite.Simple
import NeatInterpolation
import Network.HTTP.Client
import Network.Wai.Logger (withStdoutLogger)
import Optics
import Relude
import System.Directory
import System.Random
import WikiMusic.Boot qualified
import WikiMusic.Model.Config

data CreatedTestUser = CreatedTestUser
  { identifier :: UUID,
    displayName :: Text,
    emailAddress :: Text,
    password :: Text,
    authToken :: Text
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''CreatedTestUser

mkTestUrl :: AppConfig -> Text
mkTestUrl cfg = "http://" <> cfg ^. #servant % #host <> ":" <> (T.pack . show $ cfg ^. #servant % #port)

httpCall :: (MonadIO m) => Text -> m (Response BL.ByteString)
httpCall url = do
  let settings = managerSetProxy (proxyEnvironment Nothing) defaultManagerSettings
  man <- liftIO $ newManager settings
  let req = (fromString . T.unpack $ url) {proxy = Nothing}
  res <- liftIO $ httpLbs req man
  _ <- print res
  pure res

httpCallWithToken :: (MonadIO m) => Text -> Text -> m (Response BL.ByteString)
httpCallWithToken token url = do
  let settings = managerSetProxy (proxyEnvironment Nothing) defaultManagerSettings
  man <- liftIO $ newManager settings
  let req = (fromString . T.unpack $ url) {proxy = Nothing, requestHeaders = [("x-wikimusic-auth", fromString . T.unpack $ token)]}
  res <- liftIO $ httpLbs req man
  _ <- print res
  pure res

testWikiMusic :: (MonadIO m) => (AppConfig -> m a) -> m a
testWikiMusic eff = do
  portNumber <- liftIO $ randomRIO (2000, 65000)
  someUUID <- liftIO nextRandom

  let dbPath = "resources/test/wikimusic-" <> (T.pack . show $ someUUID) <> ".sqlite"
      cfg = mkConfig portNumber dbPath
      startWikiMusic = withStdoutLogger $ \logger' ->
        WikiMusic.Boot.startWikiMusicAPI logger' cfg

  processThread <- liftIO $ forkIO startWikiMusic
  result <- eff cfg
  _ <- liftIO $ killThread processThread
  _ <- liftIO . removeFile . fromString . T.unpack $ dbPath
  pure result

mkConfig :: Int -> Text -> AppConfig
mkConfig portNumber dbPath =
  AppConfig
    { servant =
        ServantConfig
          { port = portNumber,
            host = "127.0.0.1"
          },
      sqlite =
        SqliteConfig
          { path = dbPath,
            runMigrations = True
          },
      cors =
        CorsConfig
          { origins = [],
            methods = [],
            requestHeaders = []
          },
      mail =
        MailConfig
          { sendTimeoutSeconds = 0,
            host = "",
            userFile = "",
            user = Nothing,
            passwordFile = "",
            password = Nothing,
            senderName = "",
            senderMail = ""
          },
      webFrontend =
        WebFrontendConfig
          { baseUrl = ""
          },
      dev =
        DevConfig
          { reportedVersion = ""
          }
    }

randomText :: (MonadIO m) => m Text
randomText = T.pack . take 16 . randomRs ('a', 'z') <$> newStdGen

createUserInDB :: (MonadIO m) => Text -> m CreatedTestUser
createUserInDB dbPath = do
  someUUID <- liftIO nextRandom
  someText <- randomText
  now <- liftIO getCurrentTime
  let mail = someText <> "@gmail.com"
      password = T.pack . reverse . T.unpack $ someText
      authToken = password <> "-" <> password
  hashed <- hashPassword (mkPassword password)
  let q =
        [trimming|
                 INSERT INTO users (identifier, display_name, email_address,
                 password_hash, auth_token, created_at) VALUES (?,?,?,?,?,?)
                 |]

  _ <- liftIO . doInDB dbPath $ \conn ->
    execute
      conn
      (fromString . T.unpack $ q)
      (T.pack . show $ someUUID, someText, mail, unPasswordHash hashed, authToken, now)
  let createdTestUser =
        CreatedTestUser
          { identifier = someUUID,
            displayName = someText,
            emailAddress = mail,
            password = password,
            authToken = authToken
          }
  _ <- print createdTestUser
  pure createdTestUser

doInDB :: (MonadIO m) => Text -> (Connection -> m a) -> m ()
doInDB dbPath eff = do
  conn <- liftIO $ open (T.unpack dbPath)
  _ <- eff conn
  liftIO $ close conn
