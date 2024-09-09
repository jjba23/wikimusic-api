{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Boot
  ( boot,
  )
where

import Control.Monad
import Data.ByteString.Lazy qualified as BL
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

boot :: (MonadIO m) => m ()
boot = liftIO $ withStdoutLogger $ \logger' ->
  ( do
      _ <- liftIO $ P.register P.ghcMetrics
      args <- liftIO getArgs
      maybeCfg <- readConfig (cfg args)
      liftIO $ either crashWithBadConfig (doRun logger') maybeCfg
  )
  where
    crashWithBadConfig e = error ("Bad config could not be parsed! " <> show e)
    doRun logger' config = do
      startWikiMusicAPI logger' config
    cfg args = case nonEmpty args of
      Just (x :| []) -> T.pack x
      _ -> "resources/config/run-local.toml"

startWikiMusicAPI :: (MonadIO m) => ApacheLogger -> AppConfig -> m ()
startWikiMusicAPI logger' cfg = do
  liftIO . BL.putStr $ "Starting REST API ..."
  liftIO $ runSettings apiSettings =<< mkApp logger' cfg
  where
    apiSettings = setPort (cfg ^. #servant % #port) defaultSettings
