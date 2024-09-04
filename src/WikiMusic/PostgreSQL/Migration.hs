{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.PostgreSQL.Migration
  ( runWikiMusicMigrations,
  )
where

import Hasql.Migration
import Hasql.Pool qualified
import Hasql.Transaction
import Hasql.Transaction.Sessions qualified as TransactionSessions
import WikiMusic.Protolude

initializeSchema :: Transaction ()
initializeSchema = do
  sql
    $ encodeUtf8
      [trimming|
      CREATE TABLE IF NOT EXISTS schema_migrations 
         (filename varchar(512) NOT NULL,
         checksum varchar(32) NOT NULL,
         executed_at timestamptz NOT NULL DEFAULT now()
        );
      |]

runMigrationFile :: (MonadIO m) => Hasql.Pool.Pool -> MigrationCommand -> m (Either Hasql.Pool.UsageError (Maybe MigrationError))
runMigrationFile pool migrationFile =
  liftIO
    $ Hasql.Pool.use
      pool
      ( TransactionSessions.transaction
          TransactionSessions.Serializable
          TransactionSessions.Write
          migrationCommand
      )
  where
    migrationCommand = runMigration migrationFile

runWikiMusicMigrations :: (MonadIO m) => Hasql.Pool.Pool -> m [Either Hasql.Pool.UsageError (Maybe MigrationError)]
runWikiMusicMigrations pool = do
  _ <-
    liftIO
      $ Hasql.Pool.use
        pool
        ( TransactionSessions.transaction
            TransactionSessions.Serializable
            TransactionSessions.Write
            initializeSchema
        )

  migrations <- liftIO $ loadMigrationsFromDirectory "resources/postgresql/migrations"
  mapM (liftIO . runMigrationFile pool) migrations
