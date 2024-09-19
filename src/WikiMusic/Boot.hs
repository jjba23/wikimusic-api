{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Boot where

import Control.Concurrent
import Control.Monad
import Data.Text qualified as T
import Database.Beam
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Optics
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import Relude
import WikiMusic.Config
import WikiMusic.Model.Config
import WikiMusic.Servant.ApiSetup
import Yggdrasil

boot :: (MonadIO m) => m ()
boot = liftIO $ withStdoutLogger $ \logger' ->
  ( do
      _ <- liftIO $ P.register P.ghcMetrics
      args <- liftIO getArgs
      maybeCfg <- readConfig (configPathFromArgs args)
      liftIO $ either crashWithBadConfig (doRun logger') maybeCfg
  )
  where
    crashWithBadConfig e = error ("Bad config could not be parsed! " <> show e)
    doRun logger' cfg = do
      liftIO . putText $ "Starting Yggdrasil migrations ..."
      _ <- forkIO $ runYggdrasil (yggdrasil cfg)
      _ <- startWikiMusicAPI logger' cfg
      pure ()
    yggdrasil cfg =
      Yggdrasil
        { databaseFilePath = cfg ^. #sqlite % #path,
          migrationsDirectoryPath = "./resources/migrations/sqlite/",
          runMigrations = True,
          engine = SQLite
        }
    configPathFromArgs args = case nonEmpty args of
      Just (x :| []) -> T.pack x
      _ -> "resources/config/run-local.toml"

startWikiMusicAPI :: (MonadIO m) => ApacheLogger -> AppConfig -> m ()
startWikiMusicAPI logger' cfg = do
  liftIO . putText $ "Starting REST API ..."
  liftIO $ runSettings apiSettings =<< mkApp logger' cfg
  where
    apiSettings = setPort (cfg ^. #servant % #port) defaultSettings
