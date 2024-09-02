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

module WikiMusic.Beam.Database where

import Database.Beam
import Optics
import WikiMusic.Beam.Artist
import WikiMusic.Beam.Genre
import WikiMusic.Beam.Relations
import WikiMusic.Beam.Song
import WikiMusic.Beam.User
import WikiMusic.Protolude

data WikiMusicDatabase f = WikiMusicDatabase
  { songs :: f (TableEntity SongT),
    songComments :: f (TableEntity SongCommentT),
    songArtworks :: f (TableEntity SongArtworkT),
    songOpinions :: f (TableEntity SongOpinionT),
    songExternalSources :: f (TableEntity SongExternalSourcesT),
    songContents :: f (TableEntity SongContentsT),
    genres :: f (TableEntity GenreT),
    genreComments :: f (TableEntity GenreCommentT),
    genreArtworks :: f (TableEntity GenreArtworkT),
    genreOpinions :: f (TableEntity GenreOpinionT),
    genreExternalSources :: f (TableEntity GenreExternalSourcesT),
    artists :: f (TableEntity ArtistT),
    artistComments :: f (TableEntity ArtistCommentT),
    artistArtworks :: f (TableEntity ArtistArtworkT),
    artistOpinions :: f (TableEntity ArtistOpinionT),
    artistExternalSources :: f (TableEntity ArtistExternalSourcesT),
    users :: f (TableEntity UserT),
    userRoles :: f (TableEntity UserRoleT),
    songArtists :: f (TableEntity SongArtistT),
    songGenres :: f (TableEntity SongGenreT),
    artistGenres :: f (TableEntity ArtistGenreT)
  }
  deriving (Generic, Database be)

makeFieldLabelsNoPrefix ''WikiMusicDatabase

wikiMusicDatabase :: DatabaseSettings be WikiMusicDatabase
wikiMusicDatabase =
  defaultDbSettings
    `withDbModification` dbModification
      { songs =
          setEntityName "songs"
            <> modifyTableFields
              songTModification,
        songComments =
          setEntityName "song_comments"
            <> modifyTableFields
              songCommentTModification,
        songArtworks =
          setEntityName "song_artworks"
            <> modifyTableFields
              songArtworkTModification,
        songOpinions =
          setEntityName "song_opinions"
            <> modifyTableFields
              songOpinionTModification,
        songExternalSources =
          setEntityName "song_external_sources"
            <> modifyTableFields
              songExternalSourcesTModification,
        songContents =
          setEntityName "song_contents"
            <> modifyTableFields songContentsTModification,
        genres =
          setEntityName "genres"
            <> modifyTableFields
              genreTModification,
        genreComments =
          setEntityName "genre_comments"
            <> modifyTableFields
              genreCommentTModification,
        genreArtworks =
          setEntityName "genre_artworks"
            <> modifyTableFields
              genreArtworkTModification,
        genreOpinions =
          setEntityName "genre_opinions"
            <> modifyTableFields
              genreOpinionTModification,
        genreExternalSources =
          setEntityName "genre_external_sources"
            <> modifyTableFields
              genreExternalSourcesTModification,
        artists =
          setEntityName "artists"
            <> modifyTableFields
              artistTModification,
        artistComments =
          setEntityName "artist_comments"
            <> modifyTableFields
              artistCommentTModification,
        artistArtworks =
          setEntityName "artist_artworks"
            <> modifyTableFields
              artistArtworkTModification,
        artistOpinions =
          setEntityName "artist_opinions"
            <> modifyTableFields
              artistOpinionTModification,
        artistExternalSources =
          setEntityName "artist_external_sources"
            <> modifyTableFields
              artistExternalSourcesTModification,
        users =
          setEntityName "users"
            <> modifyTableFields
              userTModification,
        userRoles =
          setEntityName "user_roles"
            <> modifyTableFields
              userRoleTModification,
        songArtists =
          setEntityName "song_artists"
            <> modifyTableFields songArtistTModification,
        songGenres =
          setEntityName "song_genres"
            <> modifyTableFields songGenreTModification,
        artistGenres =
          setEntityName "artist_genres"
            <> modifyTableFields artistGenreTModification
      }
