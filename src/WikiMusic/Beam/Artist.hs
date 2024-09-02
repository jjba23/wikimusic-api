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

module WikiMusic.Beam.Artist where

import Data.Map qualified as Map
import Database.Beam
import Optics
import WikiMusic.Beam.Util
import WikiMusic.Model.Artist
import WikiMusic.Protolude

data ArtistT f = Artist'
  { identifier :: C f UUID,
    displayName :: C f Text,
    createdBy :: C f UUID,
    visibilityStatus :: C f Int64,
    approvedBy :: C f (Maybe UUID),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime),
    viewCount :: C f Int64,
    description :: C f (Maybe Text)
  }
  deriving (Generic, Beamable)

makeFieldLabelsNoPrefix ''ArtistT

type Artist' = ArtistT Identity

instance Table ArtistT where
  data PrimaryKey ArtistT f = ArtistId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = ArtistId . (^. #identifier)

fromArtistPk :: PrimaryKey ArtistT f -> Columnar f UUID
fromArtistPk (ArtistId i) = i

toArtist :: Artist' -> ExternalSources -> (UUID, Artist)
toArtist x ex =
  ( x ^. #identifier,
    Artist
      { identifier = x ^. #identifier,
        displayName = x ^. #displayName,
        createdBy = x ^. #createdBy,
        visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
        approvedBy = x ^. #approvedBy,
        createdAt = x ^. #createdAt,
        lastEditedAt = x ^. #lastEditedAt,
        viewCount = fromIntegral $ x ^. #viewCount,
        description = x ^. #description,
        artworks = Map.empty,
        comments = [],
        opinions = Map.empty,
        spotifyUrl = ex ^. #spotifyUrl,
        youtubeUrl = ex ^. #youtubeUrl,
        soundcloudUrl = ex ^. #soundcloudUrl,
        wikipediaUrl = ex ^. #wikipediaUrl
      }
  )

toPersistenceArtist :: Artist -> Artist'
toPersistenceArtist x =
  Artist'
    { identifier = x ^. #identifier,
      displayName = x ^. #displayName,
      createdBy = x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      approvedBy = x ^. #approvedBy,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt,
      viewCount = fromIntegral $ x ^. #viewCount,
      description = x ^. #description
    }

data ArtistCommentT f = ArtistComment'
  { identifier :: C f UUID,
    parentIdentifier :: C f (Maybe UUID),
    artistIdentifier :: PrimaryKey ArtistT f,
    createdBy :: C f UUID,
    visibilityStatus :: C f Int64,
    contents :: C f Text,
    approvedBy :: C f (Maybe UUID),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type ArtistComment' = ArtistCommentT Identity

instance Table ArtistCommentT where
  data PrimaryKey ArtistCommentT f = ArtistCommentId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = ArtistCommentId . (^. #identifier)

makeFieldLabelsNoPrefix ''ArtistCommentT

toArtistComment :: ArtistComment' -> (UUID, ArtistComment)
toArtistComment x =
  ( x ^. #identifier,
    ArtistComment
      { artistIdentifier = fromArtistPk $ x ^. #artistIdentifier,
        comment = fromPersistenceComment x
      }
  )

toPersistenceArtistComment :: ArtistComment -> ArtistComment'
toPersistenceArtistComment x =
  ArtistComment'
    { identifier = x ^. #comment % #identifier,
      parentIdentifier = x ^. #comment % #parentIdentifier,
      artistIdentifier = ArtistId $ x ^. #artistIdentifier,
      createdBy = x ^. #comment % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #comment % #visibilityStatus,
      contents = x ^. #comment % #contents,
      approvedBy = x ^. #comment % #approvedBy,
      createdAt = x ^. #comment % #createdAt,
      lastEditedAt = x ^. #comment % #lastEditedAt
    }

data ArtistArtworkT f = ArtistArtwork'
  { identifier :: C f UUID,
    artistIdentifier :: PrimaryKey ArtistT f,
    createdBy :: C f UUID,
    visibilityStatus :: C f Int64,
    approvedBy :: C f (Maybe UUID),
    contentUrl :: C f Text,
    contentCaption :: C f (Maybe Text),
    orderValue :: C f Int64,
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type ArtistArtwork' = ArtistArtworkT Identity

instance Table ArtistArtworkT where
  data PrimaryKey ArtistArtworkT f = ArtistArtworkId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = ArtistArtworkId . (^. #identifier)

makeFieldLabelsNoPrefix ''ArtistArtworkT

toArtistArtwork :: ArtistArtwork' -> (UUID, ArtistArtwork)
toArtistArtwork x =
  ( x ^. #identifier,
    ArtistArtwork
      { artistIdentifier = fromArtistPk $ x ^. #artistIdentifier,
        artwork = fromPersistenceArtwork x
      }
  )

mkArtistArtworkP :: ArtistArtwork -> ArtistArtwork'
mkArtistArtworkP x =
  ArtistArtwork'
    { identifier = x ^. #artwork % #identifier,
      artistIdentifier = ArtistId $ x ^. #artistIdentifier,
      createdBy = x ^. #artwork % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #artwork % #visibilityStatus,
      contentUrl = x ^. #artwork % #contentUrl,
      contentCaption = x ^. #artwork % #contentCaption,
      orderValue = fromIntegral $ x ^. #artwork % #orderValue,
      approvedBy = x ^. #artwork % #approvedBy,
      createdAt = x ^. #artwork % #createdAt,
      lastEditedAt = x ^. #artwork % #lastEditedAt
    }

data ArtistOpinionT f = ArtistOpinion'
  { identifier :: C f UUID,
    artistIdentifier :: PrimaryKey ArtistT f,
    createdBy :: C f UUID,
    isLike :: C f Bool,
    isDislike :: C f Bool,
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type ArtistOpinion' = ArtistOpinionT Identity

instance Table ArtistOpinionT where
  data PrimaryKey ArtistOpinionT f = ArtistOpinionId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = ArtistOpinionId . (^. #identifier)

makeFieldLabelsNoPrefix ''ArtistOpinionT

toArtistOpinion :: ArtistOpinion' -> (UUID, ArtistOpinion)
toArtistOpinion x =
  ( x ^. #identifier,
    ArtistOpinion
      { artistIdentifier = fromArtistPk $ x ^. #artistIdentifier,
        opinion = fromPersistenceOpinion x
      }
  )

data ArtistExternalSourcesT f = ArtistExternalSources'
  { identifier :: C f UUID,
    artistIdentifier :: PrimaryKey ArtistT f,
    createdBy :: C f UUID,
    spotifyUrl :: C f (Maybe Text),
    youtubeUrl :: C f (Maybe Text),
    soundcloudUrl :: C f (Maybe Text),
    wikipediaUrl :: C f (Maybe Text),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type ArtistExternalSources' = ArtistExternalSourcesT Identity

instance Table ArtistExternalSourcesT where
  data PrimaryKey ArtistExternalSourcesT f = ArtistExternalSourcesId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = ArtistExternalSourcesId . (^. #identifier)

makeFieldLabelsNoPrefix ''ArtistExternalSourcesT

toPersistenceArtistExternalContents :: Artist -> UUID -> ArtistExternalSources'
toPersistenceArtistExternalContents x newIdentifier =
  ArtistExternalSources'
    { identifier = newIdentifier,
      artistIdentifier = ArtistId $ x ^. #identifier,
      createdBy = x ^. #createdBy,
      spotifyUrl = x ^. #spotifyUrl,
      youtubeUrl = x ^. #youtubeUrl,
      soundcloudUrl = x ^. #soundcloudUrl,
      wikipediaUrl = x ^. #wikipediaUrl,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

mkArtistExSourcesP :: ArtistExternalSources -> ArtistExternalSources'
mkArtistExSourcesP x =
  ArtistExternalSources'
    { identifier = x ^. #identifier,
      artistIdentifier = ArtistId $ x ^. #artistIdentifier,
      createdBy = x ^. #createdBy,
      spotifyUrl = x ^. #spotifyUrl,
      youtubeUrl = x ^. #youtubeUrl,
      soundcloudUrl = x ^. #soundcloudUrl,
      wikipediaUrl = x ^. #wikipediaUrl,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

artistTModification :: ArtistT (FieldModification (TableField ArtistT))
artistTModification =
  tableModification
    { identifier = "identifier",
      displayName = "display_name",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      approvedBy = "approved_by",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at",
      viewCount = "views",
      description = "description"
    }

artistCommentTModification :: ArtistCommentT (FieldModification (TableField ArtistCommentT))
artistCommentTModification =
  tableModification
    { identifier = "identifier",
      artistIdentifier = ArtistId "artist_identifier",
      parentIdentifier = "parent_identifier",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      contents = "contents",
      approvedBy = "approved_by",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

artistArtworkTModification :: ArtistArtworkT (FieldModification (TableField ArtistArtworkT))
artistArtworkTModification =
  tableModification
    { identifier = "identifier",
      artistIdentifier = ArtistId "artist_identifier",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      approvedBy = "approved_by",
      contentUrl = "content_url",
      contentCaption = "content_caption",
      orderValue = "order_value",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

artistOpinionTModification :: ArtistOpinionT (FieldModification (TableField ArtistOpinionT))
artistOpinionTModification =
  tableModification
    { identifier = "identifier",
      artistIdentifier = ArtistId "artist_identifier",
      createdBy = "created_by",
      isLike = "is_like",
      isDislike = "is_dislike",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

artistExternalSourcesTModification :: ArtistExternalSourcesT (FieldModification (TableField ArtistExternalSourcesT))
artistExternalSourcesTModification =
  tableModification
    { identifier = "identifier",
      artistIdentifier = ArtistId "artist_identifier",
      createdBy = "created_by",
      spotifyUrl = "spotify_url",
      youtubeUrl = "youtube_url",
      soundcloudUrl = "soundcloud_url",
      wikipediaUrl = "wikipedia_url",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

mkArtistOpinionP :: ArtistOpinion -> ArtistOpinion'
mkArtistOpinionP x =
  ArtistOpinion'
    { identifier = x ^. #opinion % #identifier,
      artistIdentifier = ArtistId $ x ^. #artistIdentifier,
      createdBy = x ^. #opinion % #createdBy,
      isLike = x ^. #opinion % #isLike,
      isDislike = x ^. #opinion % #isDislike,
      createdAt = x ^. #opinion % #createdAt,
      lastEditedAt = x ^. #opinion % #lastEditedAt
    }
