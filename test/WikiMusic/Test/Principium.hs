{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WikiMusic.Test.Principium
  ( --
    module Relude,
    module Optics,
    module Test.Hspec,
    module WikiMusic.Model.Other,
    module NeatInterpolation,
    module Data.Time,
    UUID.UUID,
    --
    SpecWith,
    maybeDecodeUtf8,
    uuidToText,
    intToText,
    unpackText,
    packText,
    filterText,
    replaceText,
    mapElems,
    mapFromList,
    emptyMap,
    mapFilter,
    setUnion,
    takeText,
    mkTestUrl,
    createUserInDB,
    doInDB,
    httpCall,
    mkTestConfig,
    describe,
    it,
    sleepSeconds,
    testWikiMusic,
    expectStatus,
    expectAllStatus,
    expectResponseBody,
  )
where

--

--

import Control.Concurrent
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Password.Bcrypt
import Data.Set qualified
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Data.UUID.V4
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import NeatInterpolation hiding (text)
import Network.HTTP.Client
  ( Response (..),
    defaultManagerSettings,
    httpLbs,
    managerSetProxy,
    newManager,
    proxy,
    proxyEnvironment,
    requestHeaders,
    responseBody,
  )
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Logger (withStdoutLogger)
import Optics hiding (uncons)
import Relude
import System.Directory
import System.Random
import Test.Hspec
import WikiMusic.Beam.Database
import WikiMusic.Beam.User
import WikiMusic.Boot qualified
import WikiMusic.Model.Auth hiding (show)
import WikiMusic.Model.Config
import WikiMusic.Model.Other
import Yggdrasil

--

maybeDecodeUtf8 :: ByteString -> Either UnicodeException Text
maybeDecodeUtf8 = decodeUtf8'

uuidToText :: UUID.UUID -> Text
uuidToText = UUID.toText

intToText :: Int -> Text
intToText = T.pack . show

unpackText :: Text -> String
unpackText = T.unpack

packText :: String -> Text
packText = T.pack

filterText :: (Char -> Bool) -> Text -> Text
filterText = T.filter

replaceText :: Text -> Text -> Text -> Text
replaceText = T.replace

mapElems :: Map k a -> [a]
mapElems = Map.elems

mapFromList :: (Ord a) => [(a, b)] -> Map a b
mapFromList = Map.fromList

emptyMap :: Map k a
emptyMap = Map.empty

mapFilter :: (a -> Bool) -> Map k a -> Map k a
mapFilter = Map.filter

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion a b = a `Data.Set.union` b

takeText :: Int -> Text -> Text
takeText = T.take

mkTestUrl :: AppConfig -> Text
mkTestUrl cfg = "http://" <> cfg ^. #servant % #host <> ":" <> (T.pack . show $ cfg ^. #servant % #port)

httpCall :: (MonadIO m) => Maybe Text -> Text -> m (Response BL.ByteString)
httpCall token url = do
  let settings = managerSetProxy (proxyEnvironment Nothing) defaultManagerSettings
  man <- liftIO $ newManager settings
  let req =
        (fromString . T.unpack $ url)
          { requestHeaders = catMaybes [maybeAuthHeader],
            proxy = Nothing
          }
  res <- liftIO $ httpLbs req man
  _ <- print res
  pure res
  where
    maybeAuthHeader = (\t -> Just ("x-wikimusic-auth", fromString . T.unpack $ t)) =<< token

testWikiMusic :: (MonadIO m) => (AppConfig -> m a) -> m a
testWikiMusic eff = do
  portNumber <- liftIO $ randomRIO (2000, 65000)
  someUUID <- liftIO nextRandom

  let dbPath = "resources/test/wikimusic-" <> (T.pack . show $ someUUID) <> ".sqlite"
      cfg = mkTestConfig portNumber dbPath
      startWikiMusic = withStdoutLogger $ \logger' ->
        WikiMusic.Boot.startWikiMusicAPI logger' cfg

  processThread <- liftIO $ forkIO startWikiMusic
  _ <- liftIO $ runYggdrasil defaultYggdrasil {databaseFilePath = dbPath}
  result <- eff cfg
  _ <- liftIO $ killThread processThread
  _ <- liftIO . removeFile . fromString . T.unpack $ dbPath
  pure result

mkTestConfig :: Int -> Text -> AppConfig
mkTestConfig portNumber dbPath =
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

randomText :: (MonadIO m) => m Text
randomText = T.pack . take 16 . randomRs ('a', 'z') <$> newStdGen

createUserInDB :: (MonadIO m) => Text -> Text -> m WikiMusicUser
createUserInDB dbPath role = do
  conn <- liftIO $ open (fromString . T.unpack $ dbPath)
  someUUID <- liftIO nextRandom
  someUUID' <- liftIO nextRandom
  someText <- liftIO randomText
  someText' <- liftIO randomText
  now <- liftIO getCurrentTime
  let mail = someText <> "@gmail.com"
      password = T.pack . reverse . T.unpack $ someText
      authToken = password <> "-" <> password
  hashed <- hashPassword (mkPassword password)
  let u =
        User'
          { identifier = T.pack . show $ someUUID,
            displayName = someText,
            emailAddress = mail,
            passwordHash = Just $ unPasswordHash hashed,
            passwordResetToken = Nothing,
            createdAt = now,
            authToken = Just authToken,
            latestLoginAt = Nothing,
            latestLoginDevice = Nothing,
            avatarUrl = Nothing,
            lastEditedAt = Nothing,
            description = Just someText'
          } ::
          User'
  let r =
        UserRole'
          { identifier = T.pack . show $ someUUID',
            userIdentifier = UserId . T.pack . show $ someUUID,
            roleId = role,
            createdAt = now
          } ::
          UserRole'
  liftIO
    . runBeamSqliteDebug putStrLn conn
    . runInsert
    . insert ((^. #users) wikiMusicDatabase)
    $ insertValues [u]
  liftIO
    . runBeamSqliteDebug putStrLn conn
    . runInsert
    . insert ((^. #userRoles) wikiMusicDatabase)
    $ insertValues [r]
  pure $ mkUserM [role] u

doInDB :: (MonadIO m) => Text -> (Connection -> m a) -> m ()
doInDB dbPath eff = do
  conn <- liftIO $ open (T.unpack dbPath)
  _ <- eff conn
  liftIO $ close conn

sleepSeconds :: (MonadIO m) => Int -> m ()
sleepSeconds x = liftIO $ threadDelay (x * 1000000)

expectStatus :: Int -> Response body -> Expectation
expectStatus x httpResponse = (statusCode . responseStatus $ httpResponse) `shouldBe` x

expectAllStatus :: Int -> [Response body] -> Expectation
expectAllStatus x httpResponses = all ((== x) . statusCode . responseStatus) httpResponses `shouldBe` True

expectResponseBody :: Text -> Response LByteString -> Expectation
expectResponseBody txt httpResponse =
  (decodeUtf8' . fromLazy . responseBody $ httpResponse)
    `shouldBe` (Right . fromString . T.unpack $ txt)
