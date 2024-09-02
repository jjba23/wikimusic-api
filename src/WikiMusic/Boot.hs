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
import Database.Beam.Postgres
import Database.Redis qualified as Redis
import Hasql.Connection qualified
import Hasql.Pool qualified
import Hasql.Session qualified
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Optics
import Relude
import WikiMusic.Config
import WikiMusic.Model.Config
import WikiMusic.Persistence.Migration
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
      pool <- makePostgresPool config
      redisConn <- makeRedisConn config
      startWikiMusicAPI logger' config pool redisConn
    cfg args = case nonEmpty args of
      Just (x :| []) -> pack x
      _ -> "resources/config/run-local.toml"

startWikiMusicAPI :: (MonadIO m) => ApacheLogger -> AppConfig -> Hasql.Pool.Pool -> Redis.Connection -> m ()
startWikiMusicAPI logger' cfg pool redisConn = do
  maybeRunMigrations
  liftIO . BL.putStr $ "Starting REST API ..."
  liftIO $ runSettings apiSettings =<< mkApp logger' cfg pool redisConn
  where
    apiSettings = setPort (cfg ^. #servant % #port) defaultSettings
    maybeRunMigrations = do
      when (cfg ^. #postgresql % #runMigrations) $ do
        liftIO . BL.putStr $ "Starting database migrations ..."
        ex <- liftIO . runWikiMusicMigrations $ pool
        liftIO . BL.putStr . fromString . show $ ex
        pure ()

makeRedisConn :: (MonadIO m) => AppConfig -> m Redis.Connection
makeRedisConn cfg = liftIO $ Redis.checkedConnect redisConnectionSettings
  where
    redisConnectionSettings =
      Redis.defaultConnectInfo
        { Redis.connectPort = Redis.PortNumber (fromIntegral $ cfg ^. #redis % #port),
          Redis.connectAuth = fmap (fromString . unpack) (cfg ^. #redis % #password)
        }

makePostgresPool :: (MonadIO m) => AppConfig -> m Hasql.Pool.Pool
makePostgresPool cfg = do
  pool <- liftIO $ Hasql.Pool.acquire poolSize 10 10 10 dbConnectionSettings

  let healthSess = Hasql.Session.sql "SELECT 1"
  s <- liftIO $ Hasql.Pool.use pool healthSess
  either (liftIO . BL.putStr . fromString . show) pure s

  pure pool
  where
    poolSize = cfg ^. #postgresql % #poolSize
    dbConnectionSettings =
      Hasql.Connection.settings
        (fromString . unpack $ cfg ^. #postgresql % #host)
        (fromIntegral $ cfg ^. #postgresql % #port)
        (fromString . unpack $ cfg ^. #postgresql % #user)
        (maybe "" (fromString . unpack) (cfg ^. #postgresql % #password))
        (fromString . unpack $ cfg ^. #postgresql % #name)
