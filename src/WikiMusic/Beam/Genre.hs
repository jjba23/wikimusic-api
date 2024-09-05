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

module WikiMusic.Beam.Genre where

import Data.Map qualified as Map
import Data.UUID qualified as UUID
import Database.Beam
import Optics
import WikiMusic.Beam.Util
import WikiMusic.Model.Genre
import WikiMusic.Protolude

data GenreT f = Genre'
  { identifier :: C f Text,
    parentIdentifier :: C f (Maybe Text),
    displayName :: C f Text,
    createdBy :: C f Text,
    visibilityStatus :: C f Int64,
    approvedBy :: C f (Maybe Text),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime),
    viewCount :: C f Int64,
    description :: C f (Maybe Text)
  }
  deriving (Generic, Beamable)

makeFieldLabelsNoPrefix ''GenreT

type Genre' = GenreT Identity

instance Table GenreT where
  data PrimaryKey GenreT f = GenreId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = GenreId . (^. #identifier)

fromGenrePk :: PrimaryKey GenreT f -> Columnar f Text
fromGenrePk (GenreId i) = i

toGenre :: Genre' -> ExternalSources -> (UUID, Genre)
toGenre x ex =
  ( textToUUID $ x ^. #identifier,
    Genre
      { identifier = textToUUID $ x ^. #identifier,
        parentIdentifier = fmap textToUUID $ x ^. #parentIdentifier,
        displayName = x ^. #displayName,
        createdBy = textToUUID $ x ^. #createdBy,
        visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
        approvedBy = fmap textToUUID $ x ^. #approvedBy,
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

toPersistenceGenre :: Genre -> Genre'
toPersistenceGenre x =
  Genre'
    { identifier = UUID.toText $ x ^. #identifier,
      parentIdentifier = fmap UUID.toText $ x ^. #parentIdentifier,
      displayName = x ^. #displayName,
      createdBy = UUID.toText $ x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      approvedBy = fmap UUID.toText $ x ^. #approvedBy,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt,
      viewCount = fromIntegral $ x ^. #viewCount,
      description = x ^. #description
    }

data GenreCommentT f = GenreComment'
  { identifier :: C f Text,
    parentIdentifier :: C f (Maybe Text),
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f Text,
    visibilityStatus :: C f Int64,
    contents :: C f Text,
    approvedBy :: C f (Maybe Text),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type GenreComment' = GenreCommentT Identity

instance Table GenreCommentT where
  data PrimaryKey GenreCommentT f = GenreCommentId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = GenreCommentId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreCommentT

toGenreComment :: GenreComment' -> (UUID, GenreComment)
toGenreComment x =
  ( textToUUID $ x ^. #identifier,
    GenreComment
      { genreIdentifier = textToUUID $ fromGenrePk $ x ^. #genreIdentifier,
        comment = fromPersistenceComment x
      }
  )

mkGenreCommentP :: GenreComment -> GenreComment'
mkGenreCommentP x =
  GenreComment'
    { identifier = UUID.toText $ x ^. #comment % #identifier,
      parentIdentifier = fmap UUID.toText $ x ^. #comment % #parentIdentifier,
      genreIdentifier = GenreId $ UUID.toText $ x ^. #genreIdentifier,
      createdBy = UUID.toText $ x ^. #comment % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #comment % #visibilityStatus,
      contents = x ^. #comment % #contents,
      approvedBy = fmap UUID.toText $ x ^. #comment % #approvedBy,
      createdAt = x ^. #comment % #createdAt,
      lastEditedAt = x ^. #comment % #lastEditedAt
    }

data GenreArtworkT f = GenreArtwork'
  { identifier :: C f Text,
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f Text,
    visibilityStatus :: C f Int64,
    approvedBy :: C f (Maybe Text),
    contentUrl :: C f Text,
    contentCaption :: C f (Maybe Text),
    orderValue :: C f Int64,
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type GenreArtwork' = GenreArtworkT Identity

instance Table GenreArtworkT where
  data PrimaryKey GenreArtworkT f = GenreArtworkId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = GenreArtworkId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreArtworkT

toGenreArtwork :: GenreArtwork' -> (UUID, GenreArtwork)
toGenreArtwork x =
  ( textToUUID $ x ^. #identifier,
    GenreArtwork
      { genreIdentifier = textToUUID $ fromGenrePk $ x ^. #genreIdentifier,
        artwork = fromPersistenceArtwork x
      }
  )

mkGenreArtworkP :: GenreArtwork -> GenreArtwork'
mkGenreArtworkP x =
  GenreArtwork'
    { identifier = UUID.toText $ x ^. #artwork % #identifier,
      genreIdentifier = GenreId $ UUID.toText $ x ^. #genreIdentifier,
      createdBy = UUID.toText $ x ^. #artwork % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #artwork % #visibilityStatus,
      contentUrl = x ^. #artwork % #contentUrl,
      contentCaption = x ^. #artwork % #contentCaption,
      orderValue = fromIntegral $ x ^. #artwork % #orderValue,
      approvedBy = fmap UUID.toText $ x ^. #artwork % #approvedBy,
      createdAt = x ^. #artwork % #createdAt,
      lastEditedAt = x ^. #artwork % #lastEditedAt
    }

data GenreOpinionT f = GenreOpinion'
  { identifier :: C f Text,
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f Text,
    isLike :: C f Bool,
    isDislike :: C f Bool,
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type GenreOpinion' = GenreOpinionT Identity

instance Table GenreOpinionT where
  data PrimaryKey GenreOpinionT f = GenreOpinionId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = GenreOpinionId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreOpinionT

toGenreOpinion :: GenreOpinion' -> (UUID, GenreOpinion)
toGenreOpinion x =
  ( textToUUID $ x ^. #identifier,
    GenreOpinion
      { genreIdentifier = textToUUID $ fromGenrePk $ x ^. #genreIdentifier,
        opinion = fromPersistenceOpinion x
      }
  )

mkGenreOpinionP :: GenreOpinion -> GenreOpinion'
mkGenreOpinionP x =
  GenreOpinion'
    { identifier = UUID.toText $ x ^. #opinion % #identifier,
      genreIdentifier = GenreId $ UUID.toText $ x ^. #genreIdentifier,
      createdBy = UUID.toText $ x ^. #opinion % #createdBy,
      isLike = x ^. #opinion % #isLike,
      isDislike = x ^. #opinion % #isDislike,
      createdAt = x ^. #opinion % #createdAt,
      lastEditedAt = x ^. #opinion % #lastEditedAt
    }

data GenreExternalSourcesT f = GenreExternalSources'
  { identifier :: C f Text,
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f Text,
    spotifyUrl :: C f (Maybe Text),
    youtubeUrl :: C f (Maybe Text),
    soundcloudUrl :: C f (Maybe Text),
    wikipediaUrl :: C f (Maybe Text),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type GenreExternalSources' = GenreExternalSourcesT Identity

instance Table GenreExternalSourcesT where
  data PrimaryKey GenreExternalSourcesT f = GenreExternalSourcesId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = GenreExternalSourcesId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreExternalSourcesT

toPersistenceGenreExternalContents :: Genre -> UUID -> GenreExternalSources'
toPersistenceGenreExternalContents x newIdentifier =
  GenreExternalSources'
    { identifier = UUID.toText $ newIdentifier,
      genreIdentifier = GenreId $ UUID.toText $ x ^. #identifier,
      createdBy = UUID.toText $ x ^. #createdBy,
      spotifyUrl = x ^. #spotifyUrl,
      youtubeUrl = x ^. #youtubeUrl,
      soundcloudUrl = x ^. #soundcloudUrl,
      wikipediaUrl = x ^. #wikipediaUrl,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

toPersistenceGenreExternalSources :: GenreExternalSources -> GenreExternalSources'
toPersistenceGenreExternalSources x =
  GenreExternalSources'
    { identifier = UUID.toText $ x ^. #identifier,
      genreIdentifier = GenreId $ UUID.toText $ x ^. #genreIdentifier,
      createdBy = UUID.toText $ x ^. #createdBy,
      spotifyUrl = x ^. #spotifyUrl,
      youtubeUrl = x ^. #youtubeUrl,
      soundcloudUrl = x ^. #soundcloudUrl,
      wikipediaUrl = x ^. #wikipediaUrl,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

genreTModification :: GenreT (FieldModification (TableField GenreT))
genreTModification =
  tableModification
    { identifier = "identifier",
      parentIdentifier = "parent_identifier",
      displayName = "display_name",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      approvedBy = "approved_by",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at",
      viewCount = "views",
      description = "description"
    }

genreCommentTModification :: GenreCommentT (FieldModification (TableField GenreCommentT))
genreCommentTModification =
  tableModification
    { identifier = "identifier",
      genreIdentifier = GenreId "genre_identifier",
      parentIdentifier = "parent_identifier",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      contents = "contents",
      approvedBy = "approved_by",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

genreArtworkTModification :: GenreArtworkT (FieldModification (TableField GenreArtworkT))
genreArtworkTModification =
  tableModification
    { identifier = "identifier",
      genreIdentifier = GenreId "genre_identifier",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      approvedBy = "approved_by",
      contentUrl = "content_url",
      contentCaption = "content_caption",
      orderValue = "order_value",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

genreOpinionTModification :: GenreOpinionT (FieldModification (TableField GenreOpinionT))
genreOpinionTModification =
  tableModification
    { identifier = "identifier",
      genreIdentifier = GenreId "genre_identifier",
      createdBy = "created_by",
      isLike = "is_like",
      isDislike = "is_dislike",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

genreExternalSourcesTModification :: GenreExternalSourcesT (FieldModification (TableField GenreExternalSourcesT))
genreExternalSourcesTModification =
  tableModification
    { identifier = "identifier",
      genreIdentifier = GenreId "genre_identifier",
      createdBy = "created_by",
      spotifyUrl = "spotify_url",
      youtubeUrl = "youtube_url",
      soundcloudUrl = "soundcloud_url",
      wikipediaUrl = "wikipedia_url",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }
