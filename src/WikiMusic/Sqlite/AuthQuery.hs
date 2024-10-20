{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.AuthQuery () where

import Data.Text qualified as T
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Sqlite
import Relude
import WikiMusic.Beam.Database
import WikiMusic.Beam.User
import WikiMusic.Free.AuthQuery
import WikiMusic.Protolude

instance Exec AuthQuery where
  execAlgebra (FetchUserForAuthCheck env email next) = do
    next =<< fetchUserForAuthCheck' env email
  execAlgebra (FetchUserFromToken env t next) = do
    next =<< fetchUserFromToken' env t
  execAlgebra (FetchMe env identifier next) = do
    next =<< fetchMe' env identifier
  execAlgebra (FetchUserRoles env identifier next) = do
    next =<< fetchUserRoles' env identifier

fetchMe' :: (MonadIO m) => Env -> UUID -> m (Either AuthQueryError (Maybe WikiMusicUser))
fetchMe' env identifier = do
  maybeUser <- liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runSelectReturningFirst . select $ do
    filter_ (\s -> (s ^. #identifier) ==. val_ (T.pack . show $ identifier))
      $ all_ ((^. #users) wikiMusicDatabase)
  case maybeUser of
    Nothing -> pure . Left $ AuthError "User did not exist"
    (Just usr) -> do
      u' <- withRoles env (mkUserM [] usr)
      pure . Right . Just $ u'

fetchUserForAuthCheck' :: (MonadIO m) => Env -> Text -> m (Either AuthQueryError (Maybe WikiMusicUser))
fetchUserForAuthCheck' env email = do
  maybeUser <- liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runSelectReturningFirst . select $ do
    filter_ (\s -> (s ^. #emailAddress) ==. val_ email)
      $ all_ ((^. #users) wikiMusicDatabase)

  case maybeUser of
    Nothing -> pure . Left $ AuthError "User did not exist"
    (Just usr) -> do
      u <- withRoles env (mkUserM [] usr)
      pure . Right . Just $ u

fetchUserFromToken' :: (MonadIO m) => Env -> Text -> m (Either AuthQueryError (Maybe WikiMusicUser))
fetchUserFromToken' env t = do
  maybeUser <- liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runSelectReturningFirst . select $ do
    filter_ (\s -> (s ^. #authToken) ==. val_ (Just t))
      $ all_ ((^. #users) wikiMusicDatabase)

  case maybeUser of
    Nothing -> pure . Left $ AuthError "User did not exist"
    (Just usr) -> do
      u <- withRoles env (mkUserM [] usr)
      pure . Right . Just $ u

fetchUserRoles' :: (MonadIO m) => Env -> UUID -> m (Either AuthQueryError [UserRole])
fetchUserRoles' env identifier = do
  userRoles <- liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runSelectReturningList . select $ do
    filter_ (\s -> (s ^. #userIdentifier) ==. (val_ . UserId $ UUID.toText identifier))
      $ all_ ((^. #userRoles) wikiMusicDatabase)

  let roles' = map (userRole . (^. #roleId)) userRoles
  pure . Right $ roles'

withRoles :: (MonadIO m) => Env -> WikiMusicUser -> m WikiMusicUser
withRoles env usr = do
  roles' <- fetchUserRoles' env (usr ^. #identifier)
  pure $ usr {roles = fromRight [] roles'}
