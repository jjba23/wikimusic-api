{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Boot
  ( boot,
  )
where

import Control.Monad
import Data.ByteString.Lazy qualified as BL
import Data.Text (pack, unpack)
import Database.Beam
import Database.Beam.Sqlite
import Database.Redis qualified as Redis
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Optics
import Relude
import WikiMusic.Config
import WikiMusic.Model.Config
import WikiMusic.Servant.ApiSetup

boot :: (MonadIO m) => m ()
boot = liftIO $ withStdoutLogger $ \logger' ->
  ( do
      args <- liftIO getArgs
      maybeCfg <- readConfig (cfg args)
      liftIO $ either crashWithBadConfig (doRun logger') maybeCfg
  )
  where
    crashWithBadConfig e = error ("Bad config could not be parsed! " <> show e)
    doRun logger' config = do
      startWikiMusicAPI logger' config
    cfg args = case nonEmpty args of
      Just (x :| []) -> pack x
      _ -> "resources/config/run-local.toml"

startWikiMusicAPI :: (MonadIO m) => ApacheLogger -> AppConfig -> Hasql.Pool.Pool -> Redis.Connection -> m ()
startWikiMusicAPI logger' cfg pool redisConn = do  
  liftIO . BL.putStr $ "Starting REST API ..."
  liftIO $ runSettings apiSettings =<< mkApp logger' cfg
  where
    apiSettings = setPort (cfg ^. #servant % #port) defaultSettings

