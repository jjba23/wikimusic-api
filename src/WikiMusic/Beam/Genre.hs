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
import Database.Beam
import Optics
import WikiMusic.Beam.Util
import WikiMusic.Model.Genre
import WikiMusic.Protolude

data GenreT f = Genre'
  { identifier :: C f UUID,
    parentIdentifier :: C f (Maybe UUID),
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

makeFieldLabelsNoPrefix ''GenreT

type Genre' = GenreT Identity

instance Table GenreT where
  data PrimaryKey GenreT f = GenreId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = GenreId . (^. #identifier)

fromGenrePk :: PrimaryKey GenreT f -> Columnar f UUID
fromGenrePk (GenreId i) = i

toGenre :: Genre' -> ExternalSources -> (UUID, Genre)
toGenre x ex =
  ( x ^. #identifier,
    Genre
      { identifier = x ^. #identifier,
        parentIdentifier = x ^. #parentIdentifier,
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

toPersistenceGenre :: Genre -> Genre'
toPersistenceGenre x =
  Genre'
    { identifier = x ^. #identifier,
      parentIdentifier = x ^. #parentIdentifier,
      displayName = x ^. #displayName,
      createdBy = x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      approvedBy = x ^. #approvedBy,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt,
      viewCount = fromIntegral $ x ^. #viewCount,
      description = x ^. #description
    }

data GenreCommentT f = GenreComment'
  { identifier :: C f UUID,
    parentIdentifier :: C f (Maybe UUID),
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f UUID,
    visibilityStatus :: C f Int64,
    contents :: C f Text,
    approvedBy :: C f (Maybe UUID),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type GenreComment' = GenreCommentT Identity

instance Table GenreCommentT where
  data PrimaryKey GenreCommentT f = GenreCommentId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = GenreCommentId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreCommentT

toGenreComment :: GenreComment' -> (UUID, GenreComment)
toGenreComment x =
  ( x ^. #identifier,
    GenreComment
      { genreIdentifier = fromGenrePk $ x ^. #genreIdentifier,
        comment = fromPersistenceComment x
      }
  )

mkGenreCommentP :: GenreComment -> GenreComment'
mkGenreCommentP x =
  GenreComment'
    { identifier = x ^. #comment % #identifier,
      parentIdentifier = x ^. #comment % #parentIdentifier,
      genreIdentifier = GenreId $ x ^. #genreIdentifier,
      createdBy = x ^. #comment % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #comment % #visibilityStatus,
      contents = x ^. #comment % #contents,
      approvedBy = x ^. #comment % #approvedBy,
      createdAt = x ^. #comment % #createdAt,
      lastEditedAt = x ^. #comment % #lastEditedAt
    }

data GenreArtworkT f = GenreArtwork'
  { identifier :: C f UUID,
    genreIdentifier :: PrimaryKey GenreT f,
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

type GenreArtwork' = GenreArtworkT Identity

instance Table GenreArtworkT where
  data PrimaryKey GenreArtworkT f = GenreArtworkId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = GenreArtworkId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreArtworkT

toGenreArtwork :: GenreArtwork' -> (UUID, GenreArtwork)
toGenreArtwork x =
  ( x ^. #identifier,
    GenreArtwork
      { genreIdentifier = fromGenrePk $ x ^. #genreIdentifier,
        artwork = fromPersistenceArtwork x
      }
  )

mkGenreArtworkP :: GenreArtwork -> GenreArtwork'
mkGenreArtworkP x =
  GenreArtwork'
    { identifier = x ^. #artwork % #identifier,
      genreIdentifier = GenreId $ x ^. #genreIdentifier,
      createdBy = x ^. #artwork % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #artwork % #visibilityStatus,
      contentUrl = x ^. #artwork % #contentUrl,
      contentCaption = x ^. #artwork % #contentCaption,
      orderValue = fromIntegral $ x ^. #artwork % #orderValue,
      approvedBy = x ^. #artwork % #approvedBy,
      createdAt = x ^. #artwork % #createdAt,
      lastEditedAt = x ^. #artwork % #lastEditedAt
    }

data GenreOpinionT f = GenreOpinion'
  { identifier :: C f UUID,
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f UUID,
    isLike :: C f Bool,
    isDislike :: C f Bool,
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type GenreOpinion' = GenreOpinionT Identity

instance Table GenreOpinionT where
  data PrimaryKey GenreOpinionT f = GenreOpinionId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = GenreOpinionId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreOpinionT

toGenreOpinion :: GenreOpinion' -> (UUID, GenreOpinion)
toGenreOpinion x =
  ( x ^. #identifier,
    GenreOpinion
      { genreIdentifier = fromGenrePk $ x ^. #genreIdentifier,
        opinion = fromPersistenceOpinion x
      }
  )

mkGenreOpinionP :: GenreOpinion -> GenreOpinion'
mkGenreOpinionP x =
  GenreOpinion'
    { identifier = x ^. #opinion % #identifier,
      genreIdentifier = GenreId $ x ^. #genreIdentifier,
      createdBy = x ^. #opinion % #createdBy,
      isLike = x ^. #opinion % #isLike,
      isDislike = x ^. #opinion % #isDislike,
      createdAt = x ^. #opinion % #createdAt,
      lastEditedAt = x ^. #opinion % #lastEditedAt
    }

data GenreExternalSourcesT f = GenreExternalSources'
  { identifier :: C f UUID,
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f UUID,
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
  data PrimaryKey GenreExternalSourcesT f = GenreExternalSourcesId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = GenreExternalSourcesId . (^. #identifier)

makeFieldLabelsNoPrefix ''GenreExternalSourcesT

toPersistenceGenreExternalContents :: Genre -> UUID -> GenreExternalSources'
toPersistenceGenreExternalContents x newIdentifier =
  GenreExternalSources'
    { identifier = newIdentifier,
      genreIdentifier = GenreId $ x ^. #identifier,
      createdBy = x ^. #createdBy,
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
    { identifier = x ^. #identifier,
      genreIdentifier = GenreId $ x ^. #genreIdentifier,
      createdBy = x ^. #createdBy,
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
