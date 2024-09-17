module WikiMusic.Sqlite.Yggdrasil where

import Data.Text qualified as T
import Database.SQLite.Simple
import Relude
import System.Directory

runYggdrasil :: (MonadIO m) => Text -> m ()
runYggdrasil migrationsPath = do
  _ <- liftIO $ withCurrentDirectory (fromString . T.unpack $ migrationsPath) $ do
    files <- listDirectory "."
    let rawFilesWithOrder = toIntOrder $ mapMaybe (ss . T.pack) files
    let sortedFiles = mapMaybe liftTupleMaybeFromFst rawFilesWithOrder
    _ <- print sortedFiles
    pure ()

  pure ()

ss :: Text -> Maybe (Text, Text)
ss filePath = liftTupleMaybeFromFst ((fmap head . nonEmpty . T.split (== '-')) filePath, filePath)

liftTupleMaybeFromFst :: (Maybe a, b) -> Maybe (a, b)
liftTupleMaybeFromFst (Nothing, _) = Nothing
liftTupleMaybeFromFst (Just j, x) = Just (j, x)

toIntOrder :: [(Text, Text)] -> [(Maybe Int, Text)]
toIntOrder = map (first (readMaybe . T.unpack))
