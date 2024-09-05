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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.SongQuery () where

import Data.Map (elems, keys)
import Data.Map qualified as Map
import Data.UUID
import Data.Vector qualified as V
import Database.Beam
import Database.Beam.Sqlite
import Free.AlaCarte
import Hasql.Decoders as D
import Hasql.Encoders as E
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import NeatInterpolation
import Optics
import Relude
import WikiMusic.Beam.Database
import WikiMusic.Beam.Song
import WikiMusic.Beam.Util
import WikiMusic.Free.SongQuery
import WikiMusic.Model.Env
import WikiMusic.Model.Other
import WikiMusic.Model.Song
import WikiMusic.Model.Thread as CommentThread

instance Exec SongQuery where
  execAlgebra (FetchSongs env sortOrder limit offset next) =
    next =<< fetchSongs' env sortOrder limit offset
  execAlgebra (FetchSongsByUUID env sortOrder identifiers next) =
    next =<< fetchSongsByUUID' env sortOrder identifiers
  execAlgebra (EnrichedSongResponse env songMap enrichSongParams next) = do
    next =<< enrichedSongResponse' env songMap enrichSongParams
  execAlgebra (FetchSongComments env identifiers next) =
    next =<< fetchSongComments' env identifiers
  execAlgebra (FetchSongOpinions env identifiers next) =
    next =<< fetchSongOpinions' env identifiers
  execAlgebra (FetchSongArtworks env identifiers next) =
    next =<< fetchSongArtworks' env identifiers
  execAlgebra (FetchSongArtists env identifiers next) = do
    next =<< fetchSongArtists' env identifiers
  execAlgebra (SearchSongs env searchInput sortOrder limit offset next) = do
    next =<< searchSongs' env searchInput sortOrder limit offset
  execAlgebra (FetchSongContents env identifiers next) =
    next =<< fetchSongContents' env identifiers

mkOrderBy AscCreatedAt = orderBy_ (asc_ . (^. #createdAt))
mkOrderBy DescCreatedAt = orderBy_ (desc_ . (^. #createdAt))
mkOrderBy AscDisplayName = orderBy_ (asc_ . (^. #displayName))
mkOrderBy DescDisplayName = orderBy_ (desc_ . (^. #displayName))
mkOrderBy AscLastEditedAt = orderBy_ (asc_ . (^. #lastEditedAt))
mkOrderBy DescLastEditedAt = orderBy_ (desc_ . (^. #lastEditedAt))

fetchSongs' :: (MonadIO m) => Env -> SongSortOrder -> Limit -> Offset -> m (Map UUID Song, [UUID])
fetchSongs' env sortOrder (Limit limit) (Offset offset) = do
  songs <- liftIO $ runBeamPostgresDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList
      . select
      . offset_ (fromIntegral offset)
      . limit_ (fromIntegral limit)
      . mkOrderBy sortOrder
      $ all_ ((^. #songs) wikiMusicDatabase)
  filledSongs env songs

fetchSongsByUUID' :: (MonadIO m) => Env -> SongSortOrder -> [UUID] -> m (Map UUID Song, [UUID])
fetchSongsByUUID' env sortOrder identifiers = do
  songs <-
    liftIO
      . runBeamPostgresDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
      . mkOrderBy sortOrder
      $ all_ ((^. #songs) wikiMusicDatabase)
  filledSongs env songs

-- | Fetch song artworks from storage
fetchSongArtworks' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongArtwork)
fetchSongArtworks' env identifiers = do
  artworks <- liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runSelectReturningList
    . select
    . orderBy_ (asc_ . (^. #orderValue))
    $ do
      songs <-
        filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
          $ all_ ((^. #songs) wikiMusicDatabase)
      oneToMany_ ((^. #songArtworks) wikiMusicDatabase) (^. #songIdentifier) songs
  pure . Map.fromList . map toSongArtwork $ artworks

-- | Fetch song comments from storage
fetchSongComments' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongComment)
fetchSongComments' env identifiers = do
  comments <- liftIO $ runBeamPostgresDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList $ select $ do
      songs <-
        filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
          $ all_ ((^. #songs) wikiMusicDatabase)
      oneToMany_ ((^. #songComments) wikiMusicDatabase) (^. #songIdentifier) songs

  pure . Map.fromList . map toSongComment $ comments

-- | Fetch song opinions from storage
fetchSongOpinions' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongOpinion)
fetchSongOpinions' env identifiers = do
  opinions <- liftIO $ runBeamPostgresDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList $ select $ do
      songs <-
        filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
          $ all_ ((^. #songs) wikiMusicDatabase)
      oneToMany_ ((^. #songOpinions) wikiMusicDatabase) (^. #songIdentifier) songs

  pure . Map.fromList . map toSongOpinion $ opinions

enrichedSongResponse' :: (MonadIO m) => Env -> Map UUID Song -> EnrichSongParams -> m (Map UUID Song)
enrichedSongResponse' env songMap enrichSongParams = do
  let isChildOf' p x = Just (p ^. #comment % #identifier) == (x ^. #comment % #parentIdentifier)
      songIds = keys songMap
      getComment =
        if enrichSongParams ^. #includeComments
          then exec @SongQuery $ fetchSongComments env songIds
          else pure $ fromList []
      getArtwork =
        if enrichSongParams ^. #includeArtworks
          then exec @SongQuery $ fetchSongArtworks env songIds
          else pure $ fromList []
      getOpinion =
        if enrichSongParams ^. #includeOpinions
          then exec @SongQuery $ fetchSongOpinions env songIds
          else pure $ fromList []
      getArtist =
        if enrichSongParams ^. #includeArtists
          then exec @SongQuery $ fetchSongArtists env songIds
          else pure $ fromList []
      getContent =
        if enrichSongParams ^. #includeContents
          then exec @SongQuery $ fetchSongContents env songIds
          else pure $ fromList []

  artworkMap <- liftIO getArtwork
  opinionMap <- liftIO getOpinion
  commentMap <- liftIO getComment
  contentMap <- liftIO getContent
  artistPerSongList <- liftIO getArtist

  let matchesSongIdentifier song = (== song ^. #identifier) . (^. #songIdentifier)
  let enrichedSongs =
        mapMap
          ( \song -> do
              let rawCommentMap = Map.filter (matchesSongIdentifier song) commentMap
                  allComments = elems rawCommentMap
                  commentThreads = map renderThread $ mkThreads allComments isChildOf' (^. #comment % #parentIdentifier)
                  relevantArtists = filter (\(songId, _, _) -> songId == song ^. #identifier) artistPerSongList

              song
                { comments = commentThreads,
                  contents = filterMap (matchesSongIdentifier song) contentMap,
                  artworks = filterMap (matchesSongIdentifier song) artworkMap,
                  opinions = filterMap (matchesSongIdentifier song) opinionMap,
                  artists = Map.fromList $ map (\(_, artistId, artistName) -> (artistId, artistName)) relevantArtists
                }
          )
          songMap
  pure enrichedSongs

fetchSongArtists' :: (MonadIO m) => Env -> [UUID] -> m [(UUID, UUID, Text)]
fetchSongArtists' env identifiers = do
  let stmt = Statement query encoder decoder True
      query =
        "SELECT song_artists.song_identifier, artists.identifier, artists.display_name FROM song_artists \
        \ INNER JOIN artists ON song_artists.artist_identifier = artists.identifier \
        \ WHERE song_identifier = ANY($1)"
      encoder =
        E.param . E.nonNullable $ (E.foldableArray . E.nonNullable $ E.uuid)
      decoder = D.rowVector vector
      vector =
        (,,)
          <$> D.column (D.nonNullable D.uuid)
          <*> D.column (D.nonNullable D.uuid)
          <*> D.column (D.nonNullable D.text)

  ReadAbstraction.persistenceReadCall
    (env ^. #pool)
    (Session.statement identifiers stmt)
    fromList
    V.toList

searchSongs' :: (MonadIO m) => Env -> SearchInput -> SongSortOrder -> Limit -> Offset -> m (Map UUID Song, [UUID])
searchSongs' env searchInput sortOrder (Limit limit) (Offset offset) = do
  songs <-
    liftIO
      . runBeamPostgresDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #displayName) `ilike_` val_ ("%" <> searchInput ^. #value <> "%"))
      . offset_ (fromIntegral offset)
      . limit_ (fromIntegral limit)
      . mkOrderBy sortOrder
      $ all_ ((^. #songs) wikiMusicDatabase)
  filledSongs env songs

fetchSongContents' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongContent)
fetchSongContents' env identifiers = do
  contents <-
    liftIO
      . runBeamPostgresDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #songIdentifier) `in_` map (val_ . SongId) identifiers)
      $ all_ ((^. #songContents) wikiMusicDatabase)
  pure $ Map.fromList $ map (\x -> (x ^. #identifier, mkSongContentsM x)) contents

-- import WikiMusic.Protolude
mapMap :: (a -> b) -> Map k a -> Map k b
mapMap = Map.map

filterMap :: (a -> Bool) -> Map k a -> Map k a
filterMap = Map.filter

filledSongs :: (MonadIO m) => Env -> [Song'] -> m (Map UUID Song, [UUID])
filledSongs env songs = do
  externalSources <- liftIO $ runBeamPostgresDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #songIdentifier) `in_` map (val_ . (\x -> SongId $ x ^. #identifier)) songs)
      $ all_ ((^. #songExternalSources) wikiMusicDatabase)
  let filledSongs' = map (withExternalSources externalSources) songs
  pure (Map.fromList filledSongs', map (^. #identifier) songs)
  where
    withExternalSources externalSources song =
      let maybeFoundExternal =
            fmap
              (fromPersistenceExternalSource . head)
              (nonEmpty $ filter (\x -> fromSongPk (x ^. #songIdentifier) == (song ^. #identifier)) externalSources)
       in toSong song (fromMaybe emptyExternalSources maybeFoundExternal)
