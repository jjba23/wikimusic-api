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

module WikiMusic.Beam.Song where

import Data.Map qualified as Map
import Database.Beam
import Optics
import WikiMusic.Beam.Util
import WikiMusic.Model.Song
import WikiMusic.Protolude

data SongT f = Song'
  { identifier :: C f UUID,
    displayName :: C f Text,
    musicKey :: C f (Maybe Text),
    musicTuning :: C f (Maybe Text),
    musicCreationDate :: C f (Maybe Text),
    albumName :: C f (Maybe Text),
    albumInfoLink :: C f (Maybe Text),
    createdBy :: C f UUID,
    visibilityStatus :: C f Int64,
    approvedBy :: C f (Maybe UUID),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime),
    viewCount :: C f Int64,
    description :: C f (Maybe Text)
  }
  deriving (Generic, Beamable)

makeFieldLabelsNoPrefix ''SongT

type Song' = SongT Identity

instance Table SongT where
  data PrimaryKey SongT f = SongId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongId . (^. #identifier)

fromSongPk :: PrimaryKey SongT f -> Columnar f UUID
fromSongPk (SongId i) = i

toSong :: Song' -> ExternalSources -> (UUID, Song)
toSong x ex =
  ( x ^. #identifier,
    Song
      { identifier = x ^. #identifier,
        musicKey = x ^. #musicKey,
        musicTuning = x ^. #musicTuning,
        musicCreationDate = x ^. #musicCreationDate,
        albumName = x ^. #albumName,
        albumInfoLink = x ^. #albumInfoLink,
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
        artists = Map.empty,
        contents = Map.empty,
        spotifyUrl = ex ^. #spotifyUrl,
        youtubeUrl = ex ^. #youtubeUrl,
        soundcloudUrl = ex ^. #soundcloudUrl,
        wikipediaUrl = ex ^. #wikipediaUrl
      }
  )

toPersistenceSong :: Song -> Song'
toPersistenceSong x =
  Song'
    { identifier = x ^. #identifier,
      musicKey = x ^. #musicKey,
      musicTuning = x ^. #musicTuning,
      musicCreationDate = x ^. #musicCreationDate,
      albumName = x ^. #albumName,
      albumInfoLink = x ^. #albumInfoLink,
      displayName = x ^. #displayName,
      createdBy = x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      approvedBy = x ^. #approvedBy,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt,
      viewCount = fromIntegral $ x ^. #viewCount,
      description = x ^. #description
    }

data SongCommentT f = SongComment'
  { identifier :: C f UUID,
    parentIdentifier :: C f (Maybe UUID),
    songIdentifier :: PrimaryKey SongT f,
    createdBy :: C f UUID,
    visibilityStatus :: C f Int64,
    contents :: C f Text,
    approvedBy :: C f (Maybe UUID),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type SongComment' = SongCommentT Identity

instance Table SongCommentT where
  data PrimaryKey SongCommentT f = SongCommentId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongCommentId . (^. #identifier)

makeFieldLabelsNoPrefix ''SongCommentT

toPersistenceSongComment :: SongComment -> SongComment'
toPersistenceSongComment x =
  SongComment'
    { identifier = x ^. #comment % #identifier,
      parentIdentifier = x ^. #comment % #parentIdentifier,
      songIdentifier = SongId $ x ^. #songIdentifier,
      createdBy = x ^. #comment % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #comment % #visibilityStatus,
      contents = x ^. #comment % #contents,
      approvedBy = x ^. #comment % #approvedBy,
      createdAt = x ^. #comment % #createdAt,
      lastEditedAt = x ^. #comment % #lastEditedAt
    }

toSongComment :: SongComment' -> (UUID, SongComment)
toSongComment x =
  ( x ^. #identifier,
    SongComment
      { songIdentifier = fromSongPk $ x ^. #songIdentifier,
        comment = fromPersistenceComment x
      }
  )

data SongArtworkT f = SongArtwork'
  { identifier :: C f UUID,
    songIdentifier :: PrimaryKey SongT f,
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

type SongArtwork' = SongArtworkT Identity

instance Table SongArtworkT where
  data PrimaryKey SongArtworkT f = SongArtworkId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongArtworkId . (^. #identifier)

makeFieldLabelsNoPrefix ''SongArtworkT

toSongArtwork :: SongArtwork' -> (UUID, SongArtwork)
toSongArtwork x =
  ( x ^. #identifier,
    SongArtwork
      { songIdentifier = fromSongPk $ x ^. #songIdentifier,
        artwork = fromPersistenceArtwork x
      }
  )

mkSongArtworkP :: SongArtwork -> SongArtwork'
mkSongArtworkP x =
  SongArtwork'
    { identifier = x ^. #artwork % #identifier,
      songIdentifier = SongId $ x ^. #songIdentifier,
      createdBy = x ^. #artwork % #createdBy,
      visibilityStatus = fromIntegral $ x ^. #artwork % #visibilityStatus,
      contentUrl = x ^. #artwork % #contentUrl,
      contentCaption = x ^. #artwork % #contentCaption,
      orderValue = fromIntegral $ x ^. #artwork % #orderValue,
      approvedBy = x ^. #artwork % #approvedBy,
      createdAt = x ^. #artwork % #createdAt,
      lastEditedAt = x ^. #artwork % #lastEditedAt
    }

data SongOpinionT f = SongOpinion'
  { identifier :: C f UUID,
    songIdentifier :: PrimaryKey SongT f,
    createdBy :: C f UUID,
    isLike :: C f Bool,
    isDislike :: C f Bool,
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type SongOpinion' = SongOpinionT Identity

instance Table SongOpinionT where
  data PrimaryKey SongOpinionT f = SongOpinionId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongOpinionId . (^. #identifier)

makeFieldLabelsNoPrefix ''SongOpinionT

toSongOpinion :: SongOpinion' -> (UUID, SongOpinion)
toSongOpinion x =
  ( x ^. #identifier,
    SongOpinion
      { songIdentifier = fromSongPk $ x ^. #songIdentifier,
        opinion = fromPersistenceOpinion x
      }
  )

data SongExternalSourcesT f = SongExternalSources'
  { identifier :: C f UUID,
    songIdentifier :: PrimaryKey SongT f,
    createdBy :: C f UUID,
    spotifyUrl :: C f (Maybe Text),
    youtubeUrl :: C f (Maybe Text),
    soundcloudUrl :: C f (Maybe Text),
    wikipediaUrl :: C f (Maybe Text),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type SongExternalSources' = SongExternalSourcesT Identity

instance Table SongExternalSourcesT where
  data PrimaryKey SongExternalSourcesT f = SongExternalSourcesId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongExternalSourcesId . (^. #identifier)

makeFieldLabelsNoPrefix ''SongExternalSourcesT

toPersistenceSongExternalContents :: Song -> UUID -> SongExternalSources'
toPersistenceSongExternalContents x newIdentifier =
  SongExternalSources'
    { identifier = newIdentifier,
      songIdentifier = SongId $ x ^. #identifier,
      createdBy = x ^. #createdBy,
      spotifyUrl = x ^. #spotifyUrl,
      youtubeUrl = x ^. #youtubeUrl,
      soundcloudUrl = x ^. #soundcloudUrl,
      wikipediaUrl = x ^. #wikipediaUrl,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

toPersistenceSongExternalSources :: SongExternalSources -> SongExternalSources'
toPersistenceSongExternalSources x =
  SongExternalSources'
    { identifier = x ^. #identifier,
      songIdentifier = SongId $ x ^. #songIdentifier,
      createdBy = x ^. #createdBy,
      spotifyUrl = x ^. #spotifyUrl,
      youtubeUrl = x ^. #youtubeUrl,
      soundcloudUrl = x ^. #soundcloudUrl,
      wikipediaUrl = x ^. #wikipediaUrl,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

songTModification :: SongT (FieldModification (TableField SongT))
songTModification =
  tableModification
    { identifier = "identifier",
      displayName = "display_name",
      createdBy = "created_by",
      musicKey = "music_key",
      musicTuning = "music_tuning",
      musicCreationDate = "music_creation_date",
      albumName = "album_name",
      albumInfoLink = "album_info_link",
      visibilityStatus = "visibility_status",
      approvedBy = "approved_by",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at",
      viewCount = "views",
      description = "description"
    }

songCommentTModification :: SongCommentT (FieldModification (TableField SongCommentT))
songCommentTModification =
  tableModification
    { identifier = "identifier",
      songIdentifier = SongId "song_identifier",
      parentIdentifier = "parent_identifier",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      contents = "contents",
      approvedBy = "approved_by",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

songArtworkTModification :: SongArtworkT (FieldModification (TableField SongArtworkT))
songArtworkTModification =
  tableModification
    { identifier = "identifier",
      songIdentifier = SongId "song_identifier",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      approvedBy = "approved_by",
      contentUrl = "content_url",
      contentCaption = "content_caption",
      orderValue = "order_value",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

songOpinionTModification :: SongOpinionT (FieldModification (TableField SongOpinionT))
songOpinionTModification =
  tableModification
    { identifier = "identifier",
      songIdentifier = SongId "song_identifier",
      createdBy = "created_by",
      isLike = "is_like",
      isDislike = "is_dislike",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

songExternalSourcesTModification :: SongExternalSourcesT (FieldModification (TableField SongExternalSourcesT))
songExternalSourcesTModification =
  tableModification
    { identifier = "identifier",
      songIdentifier = SongId "song_identifier",
      createdBy = "created_by",
      spotifyUrl = "spotify_url",
      youtubeUrl = "youtube_url",
      soundcloudUrl = "soundcloud_url",
      wikipediaUrl = "wikipedia_url",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

mkSongOpinionP :: SongOpinion -> SongOpinion'
mkSongOpinionP x =
  SongOpinion'
    { identifier = x ^. #opinion % #identifier,
      songIdentifier = SongId $ x ^. #songIdentifier,
      createdBy = x ^. #opinion % #createdBy,
      isLike = x ^. #opinion % #isLike,
      isDislike = x ^. #opinion % #isDislike,
      createdAt = x ^. #opinion % #createdAt,
      lastEditedAt = x ^. #opinion % #lastEditedAt
    }

data SongContentsT f = SongContents'
  { identifier :: C f UUID,
    songIdentifier :: PrimaryKey SongT f,
    versionName :: C f Text,
    createdBy :: C f UUID,
    visibilityStatus :: C f Int64,
    approvedBy :: C f (Maybe UUID),
    instrumentType :: C f Text,
    asciiLegend :: C f (Maybe Text),
    asciiContents :: C f (Maybe Text),
    pdfContents :: C f (Maybe Text),
    guitarProContents :: C f (Maybe Text),
    createdAt :: C f UTCTime,
    lastEditedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type SongContents' = SongContentsT Identity

instance Table SongContentsT where
  data PrimaryKey SongContentsT f = SongContentsId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongContentsId . (^. #identifier)

makeFieldLabelsNoPrefix ''SongContentsT

songContentsTModification :: SongContentsT (FieldModification (TableField SongContentsT))
songContentsTModification =
  tableModification
    { identifier = "identifier",
      songIdentifier = SongId "song_identifier",
      versionName = "version_name",
      createdBy = "created_by",
      visibilityStatus = "visibility_status",
      approvedBy = "approved_by",
      instrumentType = "instrument_type",
      asciiLegend = "ascii_legend",
      asciiContents = "ascii_contents",
      pdfContents = "pdf_contents",
      guitarProContents = "guitarpro_contents",
      createdAt = "created_at",
      lastEditedAt = "last_edited_at"
    }

mkSongContentsP :: SongContent -> SongContents'
mkSongContentsP x =
  SongContents'
    { identifier = x ^. #identifier,
      songIdentifier = SongId $ x ^. #songIdentifier,
      versionName = x ^. #versionName,
      createdBy = x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      approvedBy = x ^. #approvedBy,
      instrumentType = x ^. #instrumentType,
      asciiLegend = x ^. #asciiLegend,
      asciiContents = x ^. #asciiContents,
      pdfContents = x ^. #pdfContents,
      guitarProContents = x ^. #guitarProContents,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }

mkSongContentsM :: SongContents' -> SongContent
mkSongContentsM x =
  SongContent
    { identifier = x ^. #identifier,
      songIdentifier = fromSongPk $ x ^. #songIdentifier,
      versionName = x ^. #versionName,
      createdBy = x ^. #createdBy,
      visibilityStatus = fromIntegral $ x ^. #visibilityStatus,
      approvedBy = x ^. #approvedBy,
      instrumentType = x ^. #instrumentType,
      asciiLegend = x ^. #asciiLegend,
      asciiContents = x ^. #asciiContents,
      pdfContents = x ^. #pdfContents,
      guitarProContents = x ^. #guitarProContents,
      createdAt = x ^. #createdAt,
      lastEditedAt = x ^. #lastEditedAt
    }
