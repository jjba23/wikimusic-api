{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.UserCommand () where

import Data.ByteString.Base64 qualified
import Data.Password.Bcrypt
import Data.Text (pack)
import Data.UUID.V4
import Database.Beam
import Database.Beam.Sqlite
import Hasql.Decoders as D
import Hasql.Encoders as E
import Hasql.Pool qualified
import Hasql.Statement (Statement (..))
import Relude
import WikiMusic.Beam.Database
import WikiMusic.Beam.User
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
              ==. val_ identifier
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
  maybeRowsAff <- liftIO $ hasqlTransaction (env ^. #pool) stmt (identifier, now)
  pure $ bimap fromHasqlUsageError (const ()) maybeRowsAff
  where
    stmt = Statement query encoder D.noResult True
    encoder =
      contrazip2
        (E.param . E.nonNullable $ E.uuid)
        (E.param . E.nonNullable $ E.timestamptz)
    query =
      encodeUtf8
        [trimming|
        UPDATE users SET password_reset_token = NULL,
        last_edited_at = $$2 WHERE identifier = $$1  
      |]

fromHasqlUsageError :: Hasql.Pool.UsageError -> UserCommandError
fromHasqlUsageError = PersistenceError . pack . Relude.show

changePassword :: (MonadIO m) => Env -> UserPassword -> UUID -> m (Either UserCommandError ())
changePassword env password identifier = do
  hashed <- hashPassword (mkPassword (password ^. #value))
  now <- liftIO getCurrentTime
  new <- liftIO nextRandom

  maybeRowsAff <- liftIO $ hasqlTransaction (env ^. #pool) stmt (unPasswordHash hashed, identifier, now, newToken now new)
  pure $ bimap fromHasqlUsageError (const ()) maybeRowsAff
  where
    stmt = Statement query encoder D.noResult True
    encoder =
      contrazip4
        (E.param . E.nonNullable $ E.text)
        (E.param . E.nonNullable $ E.uuid)
        (E.param . E.nonNullable $ E.timestamptz)
        (E.param . E.nonNullable $ E.text)
    query =
      encodeUtf8
        [trimming|
        UPDATE users SET password_hash = $$1, last_edited_at = $$3,
        auth_token = $$4 WHERE identifier = $$2
      |]

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
  maybeRowsAff <-
    liftIO
      $ hasqlTransaction (env ^. #pool) stmt (identifier, name' ^. #value, email ^. #value, pack . Relude.show $ identifier, now, Just (newToken now new), desc)
  let creation = bimap fromHasqlUsageError (const ()) maybeRowsAff
  case creation of
    Left e -> do
      _ <- liftIO $ putTextLn . pack . Relude.show $ e
      pure . Left $ e
    Right _ -> do
      someUUID <- liftIO nextRandom
      maybeRowsAff' <-
        liftIO
          $ hasqlTransaction (env ^. #pool) roleStmt (someUUID, identifier, pack . Relude.show $ role, now)
      let rol = bimap fromHasqlUsageError (const ()) maybeRowsAff'
      case rol of
        Left e -> do
          _ <- liftIO $ putTextLn . pack . Relude.show $ e
          pure . Left $ e
        Right _ -> doIfUserFoundByEmail env email (makeToken env)
  where
    stmt = Statement query encoder D.noResult True
    roleStmt = Statement roleQuery roleEncoder D.noResult True
    encoder =
      contrazip7
        (E.param . E.nonNullable $ E.uuid)
        (E.param . E.nonNullable $ E.text)
        (E.param . E.nonNullable $ E.text)
        (E.param . E.nonNullable $ E.text)
        (E.param . E.nonNullable $ E.timestamptz)
        (E.param . E.nullable $ E.text)
        (E.param . E.nullable $ E.text)
    roleEncoder =
      contrazip4
        (E.param . E.nonNullable $ E.uuid)
        (E.param . E.nonNullable $ E.uuid)
        (E.param . E.nonNullable $ E.text)
        (E.param . E.nonNullable $ E.timestamptz)
    bindParams' = bindParams 7
    roleBindParams' = bindParams 4
    query =
      encodeUtf8
        [trimming|
                 INSERT INTO users (identifier, display_name, email_address, password_hash, created_at, auth_token, description)
                 VALUES ( $bindParams' )
                 |]
    roleQuery =
      encodeUtf8
        [trimming|
               INSERT INTO user_roles (identifier, user_identifier, role_id, created_at)
               VALUES( $roleBindParams' )
      |]

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
    Just u -> eff (u ^. #identifier)

deleteU :: (MonadIO m) => Env -> UUID -> m (Either UserCommandError ())
deleteU env identifier = do
  maybeUser <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningFirst $ select $ do
      filter_
        ( \s ->
            (s ^. #identifier)
              ==. val_ identifier
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
