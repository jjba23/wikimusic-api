{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.AuthQuery
  ( AuthQuery (..),
    fetchUserForAuthCheck,
    fetchMe,
    fetchUserRoles,
    fetchUserFromToken,
    AuthQueryError (..),
  )
where

import Free.AlaCarte
import WikiMusic.Protolude

data AuthQueryError = PersistenceError Text | LogicError Text | AuthError Text
  deriving (Eq, Show)

type AuthQuery :: Type -> Type
data AuthQuery a
  = FetchUserForAuthCheck Env Text (Either AuthQueryError (Maybe WikiMusicUser) -> a)
  | FetchUserFromToken Env Text (Either AuthQueryError (Maybe WikiMusicUser) -> a)
  | FetchMe Env UUID (Either AuthQueryError (Maybe WikiMusicUser) -> a)
  | FetchUserRoles Env UUID (Either AuthQueryError [UserRole] -> a)
  deriving (Functor)

fetchUserForAuthCheck :: (AuthQuery :<: f) => Env -> Text -> Free f (Either AuthQueryError (Maybe WikiMusicUser))
fetchUserForAuthCheck env token = injectFree (FetchUserForAuthCheck env token Pure)

fetchUserFromToken :: (AuthQuery :<: f) => Env -> Text -> Free f (Either AuthQueryError (Maybe WikiMusicUser))
fetchUserFromToken env token = injectFree (FetchUserFromToken env token Pure)

fetchMe :: (AuthQuery :<: f) => Env -> UUID -> Free f (Either AuthQueryError (Maybe WikiMusicUser))
fetchMe env uid = injectFree (FetchMe env uid Pure)

fetchUserRoles :: (AuthQuery :<: f) => Env -> UUID -> Free f (Either AuthQueryError [UserRole])
fetchUserRoles env uid = injectFree (FetchUserRoles env uid Pure)
