{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.UserQuery () where

import Database.Beam
import Database.Beam.Sqlite
import Optics
import Relude
import WikiMusic.Beam.Database
import WikiMusic.Free.UserQuery
import WikiMusic.Protolude

instance Exec UserQuery where
  execAlgebra (DoesTokenMatchByEmail env email token next) = next =<< doesTokenMatchByEmail' env email token

doesTokenMatchByEmail' :: (MonadIO m) => Env -> UserEmail -> UserToken -> m (Either UserQueryError Bool)
doesTokenMatchByEmail' env email token = do
  maybeUser <- liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runSelectReturningFirst
    . select
    $ do
      filter_
        ( \s ->
            ((s ^. #emailAddress) ==. val_ (email ^. #value))
              &&. ((s ^. #passwordResetToken) ==. val_ (Just (token ^. #value)))
        )
        $ all_ ((^. #users) wikiMusicDatabase)

  pure . Right $ isJust maybeUser

-- getUsers' :: (MonadIO m) => Env -> m (Either UserQueryError [User])
-- getUsers' env = do
--   users <- liftIO
--     . runBeamSqliteDebug putStrLn (env ^. #conn)
--     . runSelectReturningList
--     . select
--     $ do
--       u <-
--         orderBy_ (asc_ . (^. #displayName))
--           $ filter_
--             (\s -> (s ^. #authToken) /=. val_ (Just ""))
--           $ all_ ((^. #users) wikiMusicDatabase)

--       r <-
--         oneToMany_ ((^. #userRoles) wikiMusicDatabase) (UserId . (^. #identifier)) u

--       pure (u, r)

--   -- let uu = (uncurry mkUserM)
--   -- pure . Right $ map uu users
--   -- let rr = groupBy ((==) `on` fst) $ users
--   -- let noneRR = mapMaybe nonEmpty rr
--   -- let noneRRR = map (\x -> do
--   --                       x
--   --                   ) noneRR
--   -- let roleMap = map (\l -> (fst . head $ l, map snd l)) . catMaybes . map (\(u, r) -> (nonEmpty u, r)) $ rr
--   pure . Right $ []
