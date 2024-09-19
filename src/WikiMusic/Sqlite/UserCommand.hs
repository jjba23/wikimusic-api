{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.UserCommand () where

import Data.ByteString.Base64 qualified
import Data.Password.Bcrypt
import Data.Text (pack)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4
import Database.Beam
import Database.Beam.Sqlite
import Relude
import WikiMusic.Beam.Database
import WikiMusic.Beam.User
import WikiMusic.Beam.Util
import WikiMusic.Free.UserCommand
import WikiMusic.Model.Auth
import WikiMusic.Model.Env
import WikiMusic.Protolude

instance Exec UserCommand where
  execAlgebra (MakeResetPasswordLink env email next) = doIfUserFoundByEmail env email (makeToken env) >>= next
  execAlgebra (ChangePasswordByEmail env email password next) = do
    next =<< doIfUserFoundByEmail env email (changePassword env password)
  execAlgebra (InvalidateResetTokenByEmail env email next) = next =<< doIfUserFoundByEmail env email (invalidateToken env)
  execAlgebra (InviteUser env _ email name' role desc next) = next =<< addUser env email name' role desc
  execAlgebra (DeleteUser env _ email next) = next =<< doIfUserFoundByEmail env email (deleteU env)

makeToken :: (MonadIO m) => Env -> UUID -> m (Either UserCommandError Text)
makeToken env identifier = do
  maybeUser <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningFirst $ select $ do
      filter_
        ( \s ->
            (s ^. #identifier)
              ==. val_ (UUID.toText identifier)
        )
        $ all_ ((^. #users) wikiMusicDatabase)
  now <- liftIO getCurrentTime
  maybeToken <- maybeGenToken
  case maybeUser of
    Nothing -> pure . Left $ PersistenceError ""
    Just x -> do
      case maybeToken of
        Left l -> pure . Left . PersistenceError . Relude.show $ l
        Right t -> do
          _ <-
            liftIO
              . runBeamSqliteDebug putStrLn (env ^. #conn)
              . runUpdate
              $ save
                ((^. #users) wikiMusicDatabase)
                (x {passwordResetToken = Just t, lastEditedAt = Just now} :: User')
          pure . Right $ t

maybeGenToken :: (MonadIO m) => m (Either UnicodeException Text)
maybeGenToken = do
  l <- pack . Relude.show <$> liftIO nextRandom
  r <- pack . Relude.show <$> liftIO nextRandom
  pure $ decodeUtf8' . Data.ByteString.Base64.encode . encodeUtf8 $ tokenConcat l r
  where
    tokenConcat l r = l <> "$" <> r

invalidateToken :: (MonadIO m) => Env -> UUID -> m (Either UserCommandError ())
invalidateToken env identifier = do
  now <- liftIO getCurrentTime
  art <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningOne $ select $ do
      filter_
        (\s -> (s ^. #identifier) ==. val_ (UUID.toText identifier))
        $ all_ ((^. #users) wikiMusicDatabase)
  case art of
    Nothing -> pure . Right $ ()
    Just foundArt -> do
      let a = foundArt {passwordResetToken = Nothing, lastEditedAt = Just now} :: User'
      liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #users) wikiMusicDatabase) $ a
      pure . Right $ ()

changePassword :: (MonadIO m) => Env -> UserPassword -> UUID -> m (Either UserCommandError ())
changePassword env password identifier = do
  hashed <- hashPassword (mkPassword (password ^. #value))
  now <- liftIO getCurrentTime
  new <- liftIO nextRandom
  art <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningOne $ select $ do
      filter_
        (\s -> (s ^. #identifier) ==. val_ (UUID.toText identifier))
        $ all_ ((^. #users) wikiMusicDatabase)
  case art of
    Nothing -> pure . Right $ ()
    Just foundArt -> do
      let a =
            foundArt
              { passwordHash = Just $ unPasswordHash hashed,
                authToken = Just $ newToken now new,
                lastEditedAt = Just now
              } ::
              User'
      liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #users) wikiMusicDatabase) $ a
      pure . Right $ ()

newToken :: UTCTime -> UUID -> Text
newToken now new =
  decodeUtf8
    . Data.ByteString.Base64.encode
    . encodeUtf8
    $ (pack . WikiMusic.Protolude.show $ now)
    <> "--"
    <> (pack . WikiMusic.Protolude.show $ new)

addUser :: (MonadIO m) => Env -> UserEmail -> UserName -> UserRole -> Maybe Text -> m (Either UserCommandError Text)
addUser env email name' role desc = do
  now <- liftIO getCurrentTime
  identifier <- liftIO nextRandom
  new <- liftIO nextRandom
  newNew <- liftIO nextRandom
  let u =
        User'
          { identifier = UUID.toText new,
            displayName = name' ^. #value,
            emailAddress = email ^. #value,
            passwordHash = Nothing,
            passwordResetToken = Just $ UUID.toText identifier,
            createdAt = now,
            authToken = Nothing,
            latestLoginAt = Nothing,
            latestLoginDevice = Nothing,
            avatarUrl = Nothing,
            lastEditedAt = Nothing,
            description = desc
          } ::
          User'
  let r =
        UserRole'
          { identifier = UUID.toText newNew,
            userIdentifier = UserId . UUID.toText $ new,
            roleId = T.pack . Relude.show $ role,
            createdAt = now
          } ::
          UserRole'
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #users) wikiMusicDatabase)
    $ insertValues [u]
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #userRoles) wikiMusicDatabase)
    $ insertValues [r]
  pure . Right . UUID.toText $ new

doIfUserFoundByEmail :: (MonadIO m) => Env -> UserEmail -> (UUID -> m (Either UserCommandError a)) -> m (Either UserCommandError a)
doIfUserFoundByEmail env email eff = do
  maybeUser <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningFirst $ select $ do
      filter_
        ( \s ->
            (s ^. #emailAddress)
              ==. val_ (email ^. #value)
        )
        $ all_ ((^. #users) wikiMusicDatabase)

  case maybeUser of
    Nothing -> pure . Left . LogicError $ "User could not be found!"
    Just u -> eff (textToUUID $ u ^. #identifier)

deleteU :: (MonadIO m) => Env -> UUID -> m (Either UserCommandError ())
deleteU env identifier = do
  maybeUser <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningFirst $ select $ do
      filter_
        ( \s ->
            (s ^. #identifier)
              ==. val_ (UUID.toText identifier)
        )
        $ all_ ((^. #users) wikiMusicDatabase)
  case maybeUser of
    Nothing -> pure . Left $ PersistenceError ""
    Just x -> do
      _ <-
        liftIO
          . runBeamSqliteDebug putStrLn (env ^. #conn)
          . runUpdate
          $ save ((^. #users) wikiMusicDatabase) (x {passwordHash = Just "", authToken = Just ""} :: User')
      pure . Right $ ()
