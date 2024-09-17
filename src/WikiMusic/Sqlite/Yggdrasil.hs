{-# LANGUAGE QuasiQuotes #-}

module WikiMusic.Sqlite.Yggdrasil (runYggdrasil) where

import Data.Text qualified as T
import Data.Time
import Data.UUID.V4
import Database.SQLite.Simple
import NeatInterpolation
import Relude
import System.Directory

runYggdrasil :: (MonadIO m) => Text -> Text -> m ()
runYggdrasil dbPath migrationsPath = do
  files <- liftIO $ listDirectory (fromString . T.unpack $ migrationsPath)
  let rawFilesWithOrder = toIntOrder $ mapMaybe (explodeMigrationPath . T.pack) files
  let sortedFiles = sortOn fst $ mapMaybe liftTupleMaybeFromFst rawFilesWithOrder
  _ <- print sortedFiles
  mapM_ (runMigration dbPath migrationsPath) sortedFiles

explodeMigrationPath :: Text -> Maybe (Text, Text)
explodeMigrationPath filePath = liftTupleMaybeFromFst ((fmap head . nonEmpty . T.split (== '-')) filePath, filePath)

liftTupleMaybeFromFst :: (Maybe a, b) -> Maybe (a, b)
liftTupleMaybeFromFst (Nothing, _) = Nothing
liftTupleMaybeFromFst (Just j, x) = Just (j, x)

toIntOrder :: [(Text, Text)] -> [(Maybe Int, Text)]
toIntOrder = map (first (readMaybe . T.unpack))

runMigration :: (MonadIO m) => Text -> Text -> (Int, Text) -> m ()
runMigration dbPath migrationsPath (ord', fPath) = do
  fc <- readFileBS (T.unpack (migrationsPath <> fPath))
  let maybeFc = decodeUtf8' fc
  case maybeFc of
    Left e -> print e >> error (show e)
    Right fc' -> do
      conn <- liftIO $ open (T.unpack dbPath)
      let qq = filter (/= "") . map T.strip . T.split (== ';') $ fc'
      _ <- liftIO $ mapM (\qqq -> execute_ conn (fromString . T.unpack $ qqq)) qq
      someUUID <- liftIO nextRandom
      now <- liftIO getCurrentTime
      let iq = [trimming|INSERT INTO yggdrasil (identifier, order_value, file_name, ran_at) VALUES (?,?,?,?)|]
      _ <- liftIO $ execute conn (fromString . T.unpack $ iq) (T.pack . show $ someUUID, ord', fPath, now)
      _ <- liftIO $ close conn
      pure ()
