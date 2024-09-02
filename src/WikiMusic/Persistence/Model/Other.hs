{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Persistence.Model.Other
  ( UserRow,
    toUserRow,
  )
where

import WikiMusic.Model.Other
import WikiMusic.Protolude

type UserRow =
  ( Text,
    Text,
    Text,
    Text,
    Maybe Text,
    Maybe UTCTime,
    Maybe Text,
    Maybe Text,
    UTCTime,
    Maybe UTCTime
  )

toUserRow :: User -> UserRow
toUserRow user =
  ( user ^. #identifier,
    user ^. #displayName,
    user ^. #emailAddress,
    user ^. #passwordHash,
    user ^. #passwordResetToken,
    user ^. #latestLoginAt,
    user ^. #latestLoginDevice,
    user ^. #avatarUrl,
    user ^. #createdAt,
    user ^. #lastEditedAt
  )
