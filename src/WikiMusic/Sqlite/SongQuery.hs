{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.SongQuery () where

import Data.Map (elems, keys)
import Data.Map qualified as Map
import Data.UUID
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Sqlite
import Free.AlaCarte
import Optics
import Relude
import WikiMusic.Beam.Artist
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
  execAlgebra (EnrichedSongResponse env songMap enrichSongParams next) = next =<< enrichedSongResponse' env songMap enrichSongParams
  execAlgebra (FetchSongComments env identifiers next) =
    next =<< fetchSongComments' env identifiers
  execAlgebra (FetchSongOpinions env identifiers next) =
    next =<< fetchSongOpinions' env identifiers
  execAlgebra (FetchSongArtworks env identifiers next) =
    next =<< fetchSongArtworks' env identifiers
  execAlgebra (FetchSongArtists env identifiers next) = next =<< fetchSongArtists' env identifiers
  execAlgebra (SearchSongs env searchInput sortOrder limit offset next) = next =<< searchSongs' env searchInput sortOrder limit offset
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
  songs <-
    liftIO
      $ runBeamSqliteDebug putStrLn (env ^. #conn)
      $ runSelectReturningList
      . select
      . limit_ (toInteger limit)
      . offset_ (toInteger offset)
      . mkOrderBy sortOrder
      $ all_ ((^. #songs) wikiMusicDatabase)

  filledSongs env songs

fetchSongsByUUID' :: (MonadIO m) => Env -> SongSortOrder -> [UUID] -> m (Map UUID Song, [UUID])
fetchSongsByUUID' env sortOrder identifiers = do
  songs <-
    liftIO
      . runBeamSqlite (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
      . mkOrderBy sortOrder
      $ all_ ((^. #songs) wikiMusicDatabase)
  filledSongs env songs

-- | Fetch song artworks from storage
fetchSongArtworks' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongArtwork)
fetchSongArtworks' env identifiers = do
  artworks <- liftIO
    . runBeamSqlite (env ^. #conn)
    . runSelectReturningList
    . select
    . orderBy_ (asc_ . (^. #orderValue))
    $ do
      songs <-
        filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
          $ all_ ((^. #songs) wikiMusicDatabase)
      oneToMany_ ((^. #songArtworks) wikiMusicDatabase) (^. #songIdentifier) songs
  pure . Map.fromList . map toSongArtwork $ artworks

-- | Fetch song comments from storage
fetchSongComments' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongComment)
fetchSongComments' env identifiers = do
  comments <- liftIO $ runBeamSqlite (env ^. #conn) $ runSelectReturningList $ select $ do
    songs <-
      filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
        $ all_ ((^. #songs) wikiMusicDatabase)
    oneToMany_ ((^. #songComments) wikiMusicDatabase) (^. #songIdentifier) songs

  pure . Map.fromList . map toSongComment $ comments

-- | Fetch song opinions from storage
fetchSongOpinions' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongOpinion)
fetchSongOpinions' env identifiers = do
  opinions <- liftIO $ runBeamSqlite (env ^. #conn) $ runSelectReturningList $ select $ do
    songs <-
      filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
        $ all_ ((^. #songs) wikiMusicDatabase)
    oneToMany_ ((^. #songOpinions) wikiMusicDatabase) (^. #songIdentifier) songs

  pure . Map.fromList . map toSongOpinion $ opinions

enrichedSongResponse' :: (MonadIO m) => Env -> Map UUID Song -> EnrichSongParams -> m (Map UUID Song)
enrichedSongResponse' env songMap enrichSongParams = do
  let isChildOf' p x = Just (p ^. #comment % #identifier) == x ^. #comment % #parentIdentifier
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
        Map.map
          ( \song -> do
              let rawCommentMap = Map.filter (matchesSongIdentifier song) commentMap
                  allComments = elems rawCommentMap
                  commentThreads = map renderThread $ mkThreads allComments isChildOf' (^. #comment % #parentIdentifier)
                  relevantArtists = filter (\(songId, _, _) -> songId == song ^. #identifier) artistPerSongList

              song
                { comments = commentThreads,
                  contents = Map.filter (matchesSongIdentifier song) contentMap,
                  artworks = Map.filter (matchesSongIdentifier song) artworkMap,
                  opinions = Map.filter (matchesSongIdentifier song) opinionMap,
                  artists = Map.fromList $ map (\(_, artistId, artistName) -> (artistId, artistName)) relevantArtists
                }
          )
          songMap
  pure enrichedSongs

fetchSongArtists' :: (MonadIO m) => Env -> [UUID] -> m [(UUID, UUID, Text)]
fetchSongArtists' env identifiers = do
  songArtists <-
    liftIO
      . runBeamSqlite (env ^. #conn)
      . runSelectReturningList
      . select
      . orderBy_ (asc_ . (^. #createdAt))
      $ filter_ (\s -> (s ^. #songIdentifier) `in_` map (val_ . SongId . UUID.toText) identifiers)
      $ all_ ((^. #songArtists) wikiMusicDatabase)

  artists <-
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . orderBy_ (asc_ . (^. #displayName))
      $ filter_ (\s -> (s ^. #identifier) `in_` map (val_ . fromArtistPk . (^. #artistIdentifier)) songArtists)
      $ all_ ((^. #artists) wikiMusicDatabase)
  let artistMap = Map.fromList $ map (\a -> (a ^. #identifier, a ^. #displayName)) artists
  pure
    $ map
      ( \sa ->
          ( textToUUID $ fromSongPk $ sa ^. #songIdentifier,
            textToUUID $ fromArtistPk $ sa ^. #artistIdentifier,
            fromMaybe "" $ artistMap Map.!? fromArtistPk (sa ^. #artistIdentifier)
          )
      )
      songArtists

searchSongs' :: (MonadIO m) => Env -> SearchInput -> SongSortOrder -> Limit -> Offset -> m (Map UUID Song, [UUID])
searchSongs' env searchInput sortOrder (Limit limit) (Offset offset) = do
  songs <-
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #displayName) `like_` val_ ("%" <> searchInput ^. #value <> "%"))
      . limit_ (fromIntegral limit)
      . offset_ (fromIntegral offset)
      . mkOrderBy sortOrder
      $ all_ ((^. #songs) wikiMusicDatabase)

  filledSongs env songs

fetchSongContents' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID SongContent)
fetchSongContents' env identifiers = do
  contents <-
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #songIdentifier) `in_` map (val_ . SongId . UUID.toText) identifiers)
      $ all_ ((^. #songContents) wikiMusicDatabase)
  pure $ Map.fromList $ map (\x -> (textToUUID $ x ^. #identifier, mkSongContentsM x)) contents

filledSongs :: (MonadIO m) => Env -> [Song'] -> m (Map UUID Song, [UUID])
filledSongs env songs = do
  externalSources <-
    liftIO
      $ runBeamSqliteDebug putStrLn (env ^. #conn)
      $ runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #songIdentifier) `in_` map (val_ . (\x -> SongId $ x ^. #identifier)) songs)
      $ all_ ((^. #songExternalSources) wikiMusicDatabase)
  let filledSongs' = map (withExternalSources externalSources) songs
  pure (Map.fromList filledSongs', map (textToUUID . (^. #identifier)) songs)
  where
    withExternalSources externalSources song =
      let maybeFoundExternal =
            fmap
              (fromPersistenceExternalSource . head)
              (nonEmpty $ filter (\x -> fromSongPk (x ^. #songIdentifier) == song ^. #identifier) externalSources)
       in toSong song (fromMaybe emptyExternalSources maybeFoundExternal)
