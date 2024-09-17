module WikiMusic.Test.Head where

import Control.Concurrent
import Data.Text qualified as T
import Data.UUID.V4
import Network.Wai.Logger (withStdoutLogger)
import Relude
import System.Random
import Test.Hspec
import WikiMusic.Boot qualified
import WikiMusic.Model.Config

headSpec :: SpecWith ()
headSpec =
  describe "head" $ do
    it "returns the first element of a non-empty list" $ do
      head (23 :| []) `shouldBe` (23 :: Int)
    it "starts the application properly" $ do
      portNumber <- liftIO $ randomRIO (2000, 9000)
      someUUID <- nextRandom
      let dbPath = "resources/test/wikimusic-" <> (T.pack . show $ someUUID) <> ".sqlite"
      let cfg =
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
                    { sendTimeoutSeconds = 3,
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
                    { baseUrl = "http://127.0.0.1:6923"
                    },
                dev =
                  DevConfig
                    { reportedVersion = "test"
                    }
              }
      let startWikiMusic = withStdoutLogger $ \logger' ->
            WikiMusic.Boot.startWikiMusicAPI logger' cfg

      bootThread <- forkIO startWikiMusic
      _ <- threadDelay 1000000
      _ <- killThread bootThread
      True `shouldBe` True
