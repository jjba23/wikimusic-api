{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Persistence.Model.Artist
  ( ArtistRow,
    ArtistCommentRow,
    ArtistArtworkRow,
    InsertArtistRow,
    ArtistOpinionRow,
    InsertArtistCommentRow,
    UpsertArtistOpinionRow,
    InsertArtistArtworkRow,
    InsertArtistExternalSourcesRow,
    artistFromRow,
  )
where

import WikiMusic.Model.Artist
import WikiMusic.Protolude

type ArtistRow = (UUID, Text, UUID, Int64, Maybe UUID, UTCTime, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Int64, Maybe Text)

type InsertArtistCommentRow = (Text, Text, Maybe Text, Text, Int64, Text, Maybe Text, UTCTime, Maybe UTCTime)

type InsertArtistArtworkRow = (Text, Text, Text, Int64, Maybe Text, Text, UTCTime, Maybe UTCTime, Maybe Text, Int64)

type UpsertArtistOpinionRow = (Text, Text, Text, Bool, Bool, UTCTime, Maybe UTCTime)

type InsertArtistRow = (Text, Text, Text, Int64, Maybe Text, UTCTime, Maybe UTCTime)

type ArtistArtworkRow = (UUID, UUID, UUID, Int64, Maybe UUID, Text, Maybe Text, UTCTime, Maybe UTCTime, Int64)

type ArtistCommentRow = (UUID, UUID, Maybe UUID, UUID, Int64, Text, Maybe Text, UTCTime, Maybe UTCTime)

type ArtistOpinionRow = (UUID, UUID, UUID, Bool, Bool, UTCTime, Maybe UTCTime)

type InsertArtistExternalSourcesRow = (Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, UTCTime, Maybe UTCTime)

artistFromRow :: ArtistRow -> Artist
artistFromRow
  ( identifier,
    displayName,
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
    Artist
      { identifier = identifier,
        displayName = displayName,
        createdBy = createdBy,
        visibilityStatus = fromIntegral visibilityStatus,
        approvedBy = approvedBy,
        createdAt = createdAt,
        lastEditedAt = lastEditedAt,
        artworks = fromList [],
        comments = [],
        opinions = fromList [],
        spotifyUrl = spotifyUrl,
        youtubeUrl = youtubeUrl,
        soundcloudUrl = soundcloudUrl,
        wikipediaUrl = wikipediaUrl,
        viewCount = fromIntegral viewCount,
        description = description
      }
