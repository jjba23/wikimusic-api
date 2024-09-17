{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.Test.Principium where

import Control.Concurrent
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.UUID.V4
import Network.HTTP.Client
import Network.Wai.Logger (withStdoutLogger)
import Optics
import Relude
import System.Random
import WikiMusic.Boot qualified
import WikiMusic.Model.Config

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

runWikiMusic :: (MonadIO m) => (AppConfig -> m a) -> m (Either Text a)
runWikiMusic eff = do
  portNumber <- liftIO $ randomRIO (2000, 65000)
  someUUID <- liftIO nextRandom
  let dbPath = "resources/test/wikimusic-" <> (T.pack . show $ someUUID) <> ".sqlite"
  let cfg = mkConfig portNumber dbPath

  let startWikiMusic = withStdoutLogger $ \logger' ->
        WikiMusic.Boot.startWikiMusicAPI logger' cfg

  bootThread <- liftIO $ forkIO startWikiMusic
  res <- eff cfg
  _ <- liftIO $ killThread bootThread
  pure . Right $ res

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
