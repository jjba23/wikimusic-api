{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Beam.User where

import Data.Text qualified as T
import Database.Beam
import Optics
import WikiMusic.Beam.Util
import WikiMusic.Model.Genre
import WikiMusic.Protolude

data UserT f = User'
  { identifier :: C f Text,
    displayName :: C f Text,
    emailAddress :: C f Text,
    passwordHash :: C f (Maybe Text),
    passwordResetToken :: C f (Maybe Text),
    authToken :: C f (Maybe Text),
    latestLoginAt :: C f (Maybe UTCTime),
    latestLoginDevice :: C f (Maybe Text),
    avatarUrl :: C f (Maybe Text),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime),
    description :: C f (Maybe Text)
  }
  deriving (Generic, Beamable)

makeFieldLabelsNoPrefix ''UserT

type User' = UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserId . (^. #identifier)

userTModification :: UserT (FieldModification (TableField UserT))
userTModification =
  tableModification
    { identifier = "identifier",
      displayName = "display_name",
      emailAddress = "email_address",
      passwordHash = "password_hash",
      passwordResetToken = "password_reset_token",
      authToken = "auth_token",
      latestLoginAt = "latest_login_at",
      latestLoginDevice = "latest_login_device",
      avatarUrl = "avatar_url",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

data UserRoleT f = UserRole'
  { identifier :: C f Text,
    userIdentifier :: PrimaryKey UserT f,
    roleId :: C f Text,
    createdAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

makeFieldLabelsNoPrefix ''UserRoleT

type UserRole' = UserRoleT Identity

instance Table UserRoleT where
  data PrimaryKey UserRoleT f = UserRoleId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserRoleId . (^. #identifier)

userRoleTModification :: UserRoleT (FieldModification (TableField UserRoleT))
userRoleTModification =
  tableModification
    { identifier = "identifier",
      userIdentifier = UserId "user_identifier",
      roleId = "role_id",
      createdAt = "created_at"
    }

userRole :: Text -> UserRole
userRole = read . T.unpack

mkUserM :: [Text] -> User' -> WikiMusicUser
mkUserM roles x =
  WikiMusicUser
    { identifier = textToUUID $ x ^. #identifier,
      displayName = x ^. #displayName,
      emailAddress = x ^. #emailAddress,
      passwordHash = x ^. #passwordHash,
      roles = map userRole roles,
      authToken = x ^. #authToken
    }
