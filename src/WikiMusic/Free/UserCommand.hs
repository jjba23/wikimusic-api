{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.UserCommand
  ( UserCommand (..),
    makeResetPasswordLink,
    changePasswordByEmail,
    invalidateResetTokenByEmail,
    inviteUser,
    deleteUser,
    UserCommandError (..),
    UserEmail (..),
    UserPassword (..),
    UserName (..),
  )
where

import WikiMusic.Protolude

newtype UserEmail = UserEmail {value :: Text}
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''UserEmail

newtype UserPassword = UserPassword {value :: Text}
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''UserPassword

newtype UserName = UserName {value :: Text}
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''UserName

data UserCommandError = PersistenceError Text | LogicError Text | NotificationError Text
  deriving (Show)

type UserCommand :: Type -> Type
data UserCommand a
  = MakeResetPasswordLink Env UserEmail (Either UserCommandError Text -> a)
  | ChangePasswordByEmail Env UserEmail UserPassword (Either UserCommandError () -> a)
  | InvalidateResetTokenByEmail Env UserEmail (Either UserCommandError () -> a)
  | InviteUser Env WikiMusicUser UserEmail UserName UserRole (Maybe Text) (Either UserCommandError Text -> a)
  | DeleteUser Env WikiMusicUser UserEmail (Either UserCommandError () -> a)
  deriving (Functor)

makeResetPasswordLink :: (UserCommand :<: f) => Env -> UserEmail -> Free f (Either UserCommandError Text)
makeResetPasswordLink env userEmail = injectFree (MakeResetPasswordLink env userEmail Pure)

changePasswordByEmail :: (UserCommand :<: f) => Env -> UserEmail -> UserPassword -> Free f (Either UserCommandError ())
changePasswordByEmail env userEmail userPass = injectFree (ChangePasswordByEmail env userEmail userPass Pure)

invalidateResetTokenByEmail :: (UserCommand :<: f) => Env -> UserEmail -> Free f (Either UserCommandError ())
invalidateResetTokenByEmail env userEmail = injectFree (InvalidateResetTokenByEmail env userEmail Pure)

inviteUser :: (UserCommand :<: f) => Env -> WikiMusicUser -> UserEmail -> UserName -> UserRole -> Maybe Text -> Free f (Either UserCommandError Text)
inviteUser env authUser userEmail userName userRole desc = injectFree (InviteUser env authUser userEmail userName userRole desc Pure)

deleteUser :: (UserCommand :<: f) => Env -> WikiMusicUser -> UserEmail -> Free f (Either UserCommandError ())
deleteUser env authUser userEmail = injectFree (DeleteUser env authUser userEmail Pure)
