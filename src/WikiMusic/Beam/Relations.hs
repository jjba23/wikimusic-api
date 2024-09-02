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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Beam.Relations where

import Database.Beam
import Optics
import WikiMusic.Beam.Artist
import WikiMusic.Beam.Genre
import WikiMusic.Beam.Song
import WikiMusic.Model.Song
import WikiMusic.Protolude

data SongArtistT f = SongArtist'
  { identifier :: C f UUID,
    songIdentifier :: PrimaryKey SongT f,
    artistIdentifier :: PrimaryKey ArtistT f,
    createdBy :: C f UUID,
    createdAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type SongArtist' = SongArtistT Identity

instance Table SongArtistT where
  data PrimaryKey SongArtistT f = SongArtistId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongArtistId . (^. #identifier)

makeFieldLabelsNoPrefix ''SongArtistT

songArtistTModification :: SongArtistT (FieldModification (TableField SongArtistT))
songArtistTModification =
  tableModification
    { identifier = "identifier",
      songIdentifier = SongId "song_identifier",
      artistIdentifier = ArtistId "artist_identifier",
      createdBy = "created_by",
      createdAt = "created_at"
    }

data SongGenreT f = SongGenre'
  { identifier :: C f UUID,
    songIdentifier :: PrimaryKey SongT f,
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f UUID,
    createdAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type SongGenre' = SongGenreT Identity

instance Table SongGenreT where
  data PrimaryKey SongGenreT f = SongGenreId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = SongGenreId . (^. #identifier)

makeFieldLabelsNoPrefix ''SongGenreT

songGenreTModification :: SongGenreT (FieldModification (TableField SongGenreT))
songGenreTModification =
  tableModification
    { identifier = "identifier",
      songIdentifier = SongId "song_identifier",
      genreIdentifier = GenreId "genre_identifier",
      createdBy = "created_by",
      createdAt = "created_at"
    }

data ArtistGenreT f = ArtistGenre'
  { identifier :: C f UUID,
    artistIdentifier :: PrimaryKey ArtistT f,
    genreIdentifier :: PrimaryKey GenreT f,
    createdBy :: C f UUID,
    createdAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type ArtistGenre' = ArtistGenreT Identity

instance Table ArtistGenreT where
  data PrimaryKey ArtistGenreT f = ArtistGenreId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = ArtistGenreId . (^. #identifier)

makeFieldLabelsNoPrefix ''ArtistGenreT

artistGenreTModification :: ArtistGenreT (FieldModification (TableField ArtistGenreT))
artistGenreTModification =
  tableModification
    { identifier = "identifier",
      artistIdentifier = ArtistId "artist_identifier",
      genreIdentifier = GenreId "genre_identifier",
      createdBy = "created_by",
      createdAt = "created_at"
    }

mkSongArtistP :: ArtistOfSong -> SongArtist'
mkSongArtistP x =
  SongArtist'
    { identifier = x ^. #identifier,
      songIdentifier = SongId $ x ^. #songIdentifier,
      artistIdentifier = ArtistId $ x ^. #artistIdentifier,
      createdBy = x ^. #createdBy,
      createdAt = x ^. #createdAt
    }

mkSongGenreP x =
  SongGenre'
    { identifier = x ^. #identifier,
      songIdentifier = SongId $ x ^. #songIdentifier,
      genreIdentifier = GenreId $ x ^. #genreIdentifier,
      createdBy = x ^. #createdBy,
      createdAt = x ^. #createdAt
    }

mkArtistGenreP x =
  ArtistGenre'
    { identifier = x ^. #identifier,
      artistIdentifier = ArtistId $ x ^. #artistIdentifier,
      genreIdentifier = GenreId $ x ^. #genreIdentifier,
      createdBy = x ^. #createdBy,
      createdAt = x ^. #createdAt
    }
