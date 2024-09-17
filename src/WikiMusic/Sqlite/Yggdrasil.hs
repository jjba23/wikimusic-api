{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WikiMusic.Sqlite.Yggdrasil where

import Data.Text qualified as T
import Data.Time
import Data.UUID.V4
import Database.SQLite.Simple
import NeatInterpolation
import Optics
import Relude
import System.Directory

data Yggdrasil = Yggdrasil
  { databaseFilePath :: Text,
    migrationsDirectoryPath :: Text,
    runMigrations :: Bool
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''Yggdrasil

defaultYggdrasil :: Yggdrasil
defaultYggdrasil =
  Yggdrasil
    { databaseFilePath = "./resources/test/db.sqlite",
      migrationsDirectoryPath = "./resources/migrations/sqlite/",
      runMigrations = True
    }

runYggdrasil :: (MonadIO m) => Yggdrasil -> m ()
runYggdrasil yggdrasil = when (yggdrasil ^. #runMigrations) $ do
  files <- liftIO $ listDirectory (fromString . T.unpack $ yggdrasil ^. #migrationsDirectoryPath)
  let rawFs = mapMaybe (explodeMigrationPath . T.pack) files
      rawFilesWithOrder = mapMaybe (liftTupleMaybeFromFst . toIntOrder) rawFs
      sortedFiles = sortOn fst rawFilesWithOrder
  _ <- print sortedFiles
  mapM_ (runMigration yggdrasil) sortedFiles
  where
    toIntOrder :: (Text, Text) -> (Maybe Int, Text)
    toIntOrder = first (readMaybe . T.unpack)

    explodeMigrationPath :: Text -> Maybe (Text, Text)
    explodeMigrationPath filePath =
      let k = (fmap head . nonEmpty . T.split (== '-')) filePath
       in liftTupleMaybeFromFst (k, filePath)

liftTupleMaybeFromFst :: (Maybe a, b) -> Maybe (a, b)
liftTupleMaybeFromFst (Nothing, _) = Nothing
liftTupleMaybeFromFst (Just j, x) = Just (j, x)

runMigration :: (MonadIO m) => Yggdrasil -> (Int, Text) -> m ()
runMigration yggdrasil (ord', fPath) = do
  f <- readFileBS (T.unpack (yggdrasil ^. #migrationsDirectoryPath <> fPath))
  let maybeFileContents = decodeUtf8' f
  case maybeFileContents of
    Left e -> print e >> error (show e) -- migration errors should be critical
    Right fileContents' -> do
      conn <- liftIO $ open (T.unpack $ yggdrasil ^. #databaseFilePath)
      -- run statements from parsed file  in order
      _ <-
        liftIO
          $ mapM
            (\qqq -> execute_ conn (fromString . T.unpack $ qqq))
            (parseTextToSqlStatements fileContents')
      -- record in migrations table that we ran migration so as to not run again
      someUUID <- liftIO nextRandom
      now <- liftIO getCurrentTime
      -- TODO: add the Yggdrasil versio number when as library
      _ <- liftIO $ execute conn (fromString . T.unpack $ insertYggdrasilMigrationQuery) (T.pack . show $ someUUID, ord', fPath, now)
      _ <- liftIO $ close conn
      pure ()
  where
    parseTextToSqlStatements = filter (/= "") . map T.strip . T.split (== ';')
    insertYggdrasilMigrationQuery =
      [trimming|
      INSERT INTO yggdrasil (identifier, order_value, file_name, ran_at) VALUES (?,?,?,?)
      |]
