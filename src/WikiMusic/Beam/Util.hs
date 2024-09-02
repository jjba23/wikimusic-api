{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module WikiMusic.Beam.Util where

import Database.Beam
import Optics
import WikiMusic.Model.Artwork
import WikiMusic.Model.Comment
import WikiMusic.Model.Opinion
import WikiMusic.Protolude

fromPersistenceArtwork x =
  Artwork
    { identifier = x ^. #identifier,
      createdBy = x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      approvedBy = x ^. #approvedBy,
      contentUrl = x ^. #contentUrl,
      contentCaption = x ^. #contentCaption,
      orderValue = fromIntegral $ x ^. #orderValue,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

fromPersistenceComment x =
  Comment
    { identifier = x ^. #identifier,
      parentIdentifier = x ^. #parentIdentifier,
      createdBy = x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      contents = x ^. #contents,
      approvedBy = x ^. #approvedBy,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

fromPersistenceOpinion x =
  Opinion
    { identifier = x ^. #identifier,
      createdBy = x ^. #createdBy,
      isLike = x ^. #isLike,
      isDislike = x ^. #isDislike,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

fromPersistenceExternalSource x =
  ExternalSources
    { spotifyUrl = x ^. #spotifyUrl,
      youtubeUrl = x ^. #youtubeUrl,
      soundcloudUrl = x ^. #soundcloudUrl,
      wikipediaUrl = x ^. #wikipediaUrl
    }

data ExternalSources = ExternalSources
  { spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''ExternalSources

emptyExternalSources :: ExternalSources
emptyExternalSources =
  ExternalSources
    { spotifyUrl = Nothing,
      youtubeUrl = Nothing,
      soundcloudUrl = Nothing,
      wikipediaUrl = Nothing
    }
