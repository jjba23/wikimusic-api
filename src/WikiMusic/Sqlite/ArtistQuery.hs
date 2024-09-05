{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.ArtistQuery () where

import Data.Map (elems, keys)
import Data.Map qualified as Map
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Sqlite
import WikiMusic.Beam.Artist
import WikiMusic.Beam.Database
import WikiMusic.Beam.Util
import WikiMusic.Free.ArtistQuery
import WikiMusic.Model.Artist
import WikiMusic.Model.Other
import WikiMusic.Model.Thread as CommentThread
import WikiMusic.Protolude

instance Exec ArtistQuery where
  execAlgebra (FetchArtists env sortOrder limit offset next) =
    next =<< fetchArtists' env sortOrder limit offset
  execAlgebra (FetchArtistsByUUID env sortOrder identifiers next) =
    next =<< fetchArtistsByUUID' env sortOrder identifiers
  execAlgebra (EnrichedArtistResponse env artistMap enrichArtistParams next) =
    next =<< enrichedArtistResponse' env artistMap enrichArtistParams
  execAlgebra (FetchArtistComments env identifiers next) =
    next =<< fetchArtistComments' env identifiers
  execAlgebra (FetchArtistOpinions env identifiers next) =
    next =<< fetchArtistOpinions' env identifiers
  execAlgebra (SearchArtists env searchInput sortOrder limit offset next) =
    next =<< searchArtists' env searchInput sortOrder limit offset
  execAlgebra (FetchArtistArtworks env identifiers next) =
    next =<< fetchArtistArtworks' env identifiers

mkOrderBy AscCreatedAt = orderBy_ (asc_ . (^. #createdAt))
mkOrderBy DescCreatedAt = orderBy_ (desc_ . (^. #createdAt))
mkOrderBy AscDisplayName = orderBy_ (asc_ . (^. #displayName))
mkOrderBy DescDisplayName = orderBy_ (desc_ . (^. #displayName))
mkOrderBy AscLastEditedAt = orderBy_ (asc_ . (^. #lastEditedAt))
mkOrderBy DescLastEditedAt = orderBy_ (desc_ . (^. #lastEditedAt))

-- | Fetch artists from storage, according to a sort order, limit and offset
fetchArtists' :: (MonadIO m) => Env -> ArtistSortOrder -> Limit -> Offset -> m (Map UUID Artist, [UUID])
fetchArtists' env sortOrder (Limit limit) (Offset offset) = do
  artists <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList
      . select
      . offset_ (fromIntegral offset)
      . limit_ (fromIntegral limit)
      . mkOrderBy sortOrder
      $ all_ ((^. #artists) wikiMusicDatabase)
  filledArtists env artists

-- | Fetch artists by UUID from storage, according to a sort order
fetchArtistsByUUID' :: (MonadIO m) => Env -> ArtistSortOrder -> [UUID] -> m (Map UUID Artist, [UUID])
fetchArtistsByUUID' env sortOrder identifiers = do
  artists <-
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
      . mkOrderBy sortOrder
      $ all_ ((^. #artists) wikiMusicDatabase)
  filledArtists env artists

-- | Fetch artist artworks from storage
fetchArtistArtworks' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID ArtistArtwork)
fetchArtistArtworks' env identifiers = do
  artworks <- liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runSelectReturningList
    . select
    . orderBy_ (asc_ . (^. #orderValue))
    $ do
      songs <-
        filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
          $ all_ ((^. #artists) wikiMusicDatabase)
      oneToMany_ ((^. #artistArtworks) wikiMusicDatabase) (^. #artistIdentifier) songs
  pure . Map.fromList . map toArtistArtwork $ artworks

-- | Enrich artists with related information, according to enrichment parameters
enrichedArtistResponse' :: (MonadIO m) => Env -> Map UUID Artist -> EnrichArtistParams -> m (Map UUID Artist)
enrichedArtistResponse' env artistMap enrichArtistParams = do
  artworkMap <- liftIO getArtwork
  opinionMap <- liftIO getOpinion
  commentMap <- liftIO getComment

  let enrichedArtists =
        mapMap
          ( \artist -> do
              let rawCommentMap = Map.filter (matchesArtistIdentifier artist) commentMap
                  allComments = elems rawCommentMap
                  commentThreads = map renderThread $ mkThreads allComments isChildOf' (^. #comment % #parentIdentifier)

              artist
                { comments = commentThreads,
                  artworks = filterMap (matchesArtistIdentifier artist) artworkMap,
                  opinions = filterMap (matchesArtistIdentifier artist) opinionMap
                }
          )
          artistMap

  pure enrichedArtists
  where
    matchesArtistIdentifier artist = (== artist ^. #identifier) . (^. #artistIdentifier)
    isChildOf' p x = Just (p ^. #comment % #identifier) == x ^. #comment % #parentIdentifier
    artistIds = keys artistMap
    getComment =
      if enrichArtistParams ^. #includeComments
        then exec @ArtistQuery $ fetchArtistComments env artistIds
        else pure $ fromList []
    getArtwork =
      if enrichArtistParams ^. #includeArtworks
        then exec @ArtistQuery $ fetchArtistArtworks env artistIds
        else pure $ fromList []
    getOpinion =
      if enrichArtistParams ^. #includeOpinions
        then exec @ArtistQuery $ fetchArtistOpinions env artistIds
        else pure $ fromList []

-- | Fetch artist comments from storage
fetchArtistComments' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID ArtistComment)
fetchArtistComments' env identifiers = do
  comments <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList $ select $ do
      artists <-
        filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
          $ all_ ((^. #artists) wikiMusicDatabase)
      oneToMany_ ((^. #artistComments) wikiMusicDatabase) (^. #artistIdentifier) artists

  pure . Map.fromList . map toArtistComment $ comments

searchArtists' :: (MonadIO m) => Env -> SearchInput -> ArtistSortOrder -> Limit -> Offset -> m (Map UUID Artist, [UUID])
searchArtists' env searchInput sortOrder (Limit limit) (Offset offset) = do
  artists <-
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #displayName) `like_` val_ ("%" <> searchInput ^. #value <> "%"))
      . offset_ (fromIntegral offset)
      . limit_ (fromIntegral limit)
      . mkOrderBy sortOrder
      $ all_ ((^. #artists) wikiMusicDatabase)
  filledArtists env artists

-- | Fetch artist opinions from storage
fetchArtistOpinions' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID ArtistOpinion)
fetchArtistOpinions' env identifiers = do
  opinions <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList $ select $ do
      artists <-
        filter_ (\s -> (s ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
          $ all_ ((^. #artists) wikiMusicDatabase)
      oneToMany_ ((^. #artistOpinions) wikiMusicDatabase) (^. #artistIdentifier) artists

  pure . Map.fromList . map toArtistOpinion $ opinions

filledArtists :: (MonadIO m) => Env -> [Artist'] -> m (Map UUID Artist, [UUID])
filledArtists env artists = do
  externalSources <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #artistIdentifier) `in_` map (val_ . (\x -> ArtistId $ x ^. #identifier)) artists)
      $ all_ ((^. #artistExternalSources) wikiMusicDatabase)
  let filledArtists' = map (withExternalSources externalSources) artists
  pure (Map.fromList filledArtists', map (textToUUID . (^. #identifier)) artists)
  where
    withExternalSources externalSources artist =
      let maybeFoundExternal =
            fmap
              (fromPersistenceExternalSource . head)
              (nonEmpty $ filter (\x -> fromArtistPk (x ^. #artistIdentifier) == (artist ^. #identifier)) externalSources)
       in toArtist artist (fromMaybe emptyExternalSources maybeFoundExternal)
