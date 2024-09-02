{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Persistence.Model.Genre
  ( GenreRow,
    GenreCommentRow,
    GenreArtworkRow,
    InsertGenreRow,
    GenreOpinionRow,
    InsertGenreCommentRow,
    UpsertGenreOpinionRow,
    InsertGenreExternalSourcesRow,
    InsertGenreArtworkRow,
    genreFromRow,
  )
where

import WikiMusic.Model.Genre
import WikiMusic.Protolude

type GenreRow = (UUID, Maybe UUID, Text, UUID, Int64, Maybe UUID, UTCTime, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Int64, Maybe Text)

type InsertGenreCommentRow = (Text, Text, Maybe Text, Text, Int64, Text, Maybe Text, UTCTime, Maybe UTCTime)

type InsertGenreArtworkRow = (Text, Text, Text, Int64, Maybe Text, Text, UTCTime, Maybe UTCTime, Maybe Text, Int64)

type UpsertGenreOpinionRow = (Text, Text, Text, Bool, Bool, UTCTime, Maybe UTCTime)

type InsertGenreRow = (Text, Text, Text, Int64, Maybe Text, UTCTime, Maybe UTCTime)

type GenreArtworkRow = (UUID, UUID, UUID, Int64, Maybe UUID, Text, UTCTime, Maybe UTCTime)

type GenreCommentRow = (UUID, UUID, Maybe UUID, UUID, Int64, Text, Maybe Text, UTCTime, Maybe UTCTime)

type GenreOpinionRow = (UUID, UUID, UUID, Bool, Bool, UTCTime, Maybe UTCTime)

type InsertGenreExternalSourcesRow = (Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, UTCTime, Maybe UTCTime)

genreFromRow :: GenreRow -> Genre
genreFromRow
  ( identifier,
    parentIdentifier,
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
    Genre
      { identifier = identifier,
        parentIdentifier = parentIdentifier,
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
