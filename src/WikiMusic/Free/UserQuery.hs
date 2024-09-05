{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.UserQuery
  ( UserQuery (..),
    doesTokenMatchByEmail,
    UserQueryError (..),
    UserEmail (..),
    UserToken (..),
  )
where

import WikiMusic.Protolude

newtype UserEmail = UserEmail {value :: Text}
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''UserEmail

newtype UserToken = UserToken {value :: Text}
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''UserToken

data UserQueryError = PersistenceError Text | LogicError Text deriving (Show)

data UserQuery a
  = DoesTokenMatchByEmail Env UserEmail UserToken (Either UserQueryError Bool -> a)
  deriving (Functor)

doesTokenMatchByEmail :: (UserQuery :<: f) => Env -> UserEmail -> UserToken -> Free f (Either UserQueryError Bool)
doesTokenMatchByEmail env userEmail userToken = injectFree (DoesTokenMatchByEmail env userEmail userToken Pure)
