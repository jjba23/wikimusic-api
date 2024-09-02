{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Persistence.Model.Song
  ( SongRow,
    SongCommentRow,
    SongArtworkRow,
    InsertSongRow,
    SongOpinionRow,
    InsertSongCommentRow,
    UpsertSongOpinionRow,
    InsertSongArtworkRow,
    SongContentRow,
    UpdateArtistsOfSongRow,
    InsertSongExternalSourcesRow,
    songFromRow,
  )
where

import WikiMusic.Model.Song
import WikiMusic.Protolude

type SongRow =
  ( UUID,
    Text,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    UUID,
    Int64,
    Maybe UUID,
    UTCTime,
    Maybe UTCTime,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    Int64,
    Maybe Text
  )

type InsertSongCommentRow = (Text, Text, Maybe Text, Text, Int64, Text, Maybe Text, UTCTime, Maybe UTCTime)

type InsertSongArtworkRow = (Text, Text, Text, Int64, Maybe Text, Text, UTCTime, Maybe UTCTime, Maybe Text, Int64)

type UpsertSongOpinionRow = (Text, Text, Text, Bool, Bool, UTCTime, Maybe UTCTime)

type InsertSongRow = (UUID, Text, UUID, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Text, Int64, Maybe Text, UTCTime, Maybe UTCTime)

type SongArtworkRow = (Text, Text, Text, Int64, Maybe Text, Text, UTCTime, Maybe UTCTime)

type SongCommentRow = (Text, Text, Maybe Text, Text, Int64, Text, Maybe Text, UTCTime, Maybe UTCTime)

type SongOpinionRow = (Text, Text, Text, Bool, Bool, UTCTime, Maybe UTCTime)

type SongContentRow = (UUID, UUID, Text, UUID, Int64, Maybe UUID, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, UTCTime, Maybe UTCTime)

type UpdateArtistsOfSongRow = (UUID, UUID, UUID, UTCTime)

type InsertSongExternalSourcesRow = (Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, UTCTime, Maybe UTCTime)

songFromRow :: SongRow -> Song
songFromRow
  ( identifier,
    displayName,
    musicKey,
    musicTuning,
    musicCreationDate,
    albumName,
    albumInfoLink,
    createdBy,
    visibilityStatus,
    approvedBy,
    createdAt,
    lastEditedAt,
    spotifyUrl,
    youtubeUrl,
    soundcloudUrl,
    wikipediaUrl,
    viewCount,
    description
    ) =
    Song
      { identifier = identifier,
        displayName = displayName,
        musicKey = musicKey,
        musicTuning = musicTuning,
        musicCreationDate = musicCreationDate,
        albumName = albumName,
        albumInfoLink = albumInfoLink,
        createdBy = createdBy,
        visibilityStatus = fromIntegral visibilityStatus,
        approvedBy = approvedBy,
        createdAt = createdAt,
        lastEditedAt = lastEditedAt,
        artworks = fromList [],
        comments = [],
        opinions = fromList [],
        contents = fromList [],
        spotifyUrl = spotifyUrl,
        youtubeUrl = youtubeUrl,
        soundcloudUrl = soundcloudUrl,
        wikipediaUrl = wikipediaUrl,
        artists = fromList [],
        viewCount = fromIntegral viewCount,
        description = description
      }
