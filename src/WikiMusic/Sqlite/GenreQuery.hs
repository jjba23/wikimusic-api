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

module WikiMusic.Sqlite.GenreQuery () where

import Data.Map (elems, keys)
import Data.Map qualified as Map
import Database.Beam
import Database.Beam.Sqlite
import WikiMusic.Beam.Database
import WikiMusic.Beam.Genre
import WikiMusic.Beam.Util
import WikiMusic.Free.GenreQuery
import WikiMusic.Model.Genre
import WikiMusic.Model.Other
import WikiMusic.Model.Thread as CommentThread
import WikiMusic.Protolude

instance Exec GenreQuery where
  execAlgebra (FetchGenres env sortOrder limit offset next) =
    next =<< fetchGenres' env sortOrder limit offset
  execAlgebra (FetchGenresByUUID env sortOrder identifiers next) =
    next =<< fetchGenresByUUID' env sortOrder identifiers
  execAlgebra (EnrichedGenreResponse env genreMap enrichGenreParams next) =
    next =<< enrichedGenreResponse' env genreMap enrichGenreParams
  execAlgebra (FetchGenreComments env identifiers next) =
    next =<< fetchGenreComments' env identifiers
  execAlgebra (FetchGenreOpinions env identifiers next) =
    next =<< fetchGenreOpinions' env identifiers
  execAlgebra (SearchGenres env searchInput sortOrder limit offset next) =
    next =<< searchGenres' env searchInput sortOrder limit offset
  execAlgebra (FetchGenreArtworks env identifiers next) =
    next =<< fetchGenreArtworks' env identifiers

mkOrderBy AscCreatedAt = orderBy_ (asc_ . (^. #createdAt))
mkOrderBy DescCreatedAt = orderBy_ (desc_ . (^. #createdAt))
mkOrderBy AscDisplayName = orderBy_ (asc_ . (^. #displayName))
mkOrderBy DescDisplayName = orderBy_ (desc_ . (^. #displayName))
mkOrderBy AscLastEditedAt = orderBy_ (asc_ . (^. #lastEditedAt))
mkOrderBy DescLastEditedAt = orderBy_ (desc_ . (^. #lastEditedAt))

-- | Fetch genres from storage, according to a sort order, limit and offset
fetchGenres' :: (MonadIO m) => Env -> GenreSortOrder -> Limit -> Offset -> m (Map UUID Genre, [UUID])
fetchGenres' env sortOrder (Limit limit) (Offset offset) = do
  genres <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList
      . select
      . offset_ (fromIntegral offset)
      . limit_ (fromIntegral limit)
      . mkOrderBy sortOrder
      $ all_ ((^. #genres) wikiMusicDatabase)
  filledGenres env genres

-- | Fetch genres by UUID from storage, according to a sort order
fetchGenresByUUID' :: (MonadIO m) => Env -> GenreSortOrder -> [UUID] -> m (Map UUID Genre, [UUID])
fetchGenresByUUID' env sortOrder identifiers = do
  genres <-
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
      . mkOrderBy sortOrder
      $ all_ ((^. #genres) wikiMusicDatabase)
  filledGenres env genres

-- | Fetch genre artworks from storage
fetchGenreArtworks' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID GenreArtwork)
fetchGenreArtworks' env identifiers = do
  artworks <- liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runSelectReturningList
    . select
    . orderBy_ (asc_ . (^. #orderValue))
    $ do
      songs <-
        filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
          $ all_ ((^. #genres) wikiMusicDatabase)
      oneToMany_ ((^. #genreArtworks) wikiMusicDatabase) (^. #genreIdentifier) songs
  pure . Map.fromList . map toGenreArtwork $ artworks

-- | Enrich genres with related information, according to enrichment parameters
enrichedGenreResponse' :: (MonadIO m) => Env -> Map UUID Genre -> EnrichGenreParams -> m (Map UUID Genre)
enrichedGenreResponse' env genreMap enrichGenreParams = do
  artworkMap <- liftIO getArtwork
  opinionMap <- liftIO getOpinion
  commentMap <- liftIO getComment

  let enrichedGenres =
        mapMap
          ( \genre -> do
              let rawCommentMap = Map.filter (matchesGenreIdentifier genre) commentMap
                  allComments = elems rawCommentMap
                  commentThreads = map renderThread $ mkThreads allComments isChildOf' (^. #comment % #parentIdentifier)

              genre
                { comments = commentThreads,
                  artworks = filterMap (matchesGenreIdentifier genre) artworkMap,
                  opinions = filterMap (matchesGenreIdentifier genre) opinionMap
                }
          )
          genreMap

  pure enrichedGenres
  where
    matchesGenreIdentifier genre = (== genre ^. #identifier) . (^. #genreIdentifier)
    isChildOf' p x = Just (p ^. #comment % #identifier) == x ^. #comment % #parentIdentifier
    genreIds = keys genreMap
    getComment =
      if enrichGenreParams ^. #includeComments
        then exec @GenreQuery $ fetchGenreComments env genreIds
        else pure $ fromList []
    getArtwork =
      if enrichGenreParams ^. #includeArtworks
        then exec @GenreQuery $ fetchGenreArtworks env genreIds
        else pure $ fromList []
    getOpinion =
      if enrichGenreParams ^. #includeOpinions
        then exec @GenreQuery $ fetchGenreOpinions env genreIds
        else pure $ fromList []

-- | Fetch genre comments from storage
fetchGenreComments' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID GenreComment)
fetchGenreComments' env identifiers = do
  comments <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList $ select $ do
      genres <-
        filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
          $ all_ ((^. #genres) wikiMusicDatabase)
      oneToMany_ ((^. #genreComments) wikiMusicDatabase) (^. #genreIdentifier) genres

  pure . Map.fromList . map toGenreComment $ comments

-- | Search genres by keywords from storage, according to a sort order, limit and offset
searchGenres' :: (MonadIO m) => Env -> SearchInput -> GenreSortOrder -> Limit -> Offset -> m (Map UUID Genre, [UUID])
searchGenres' env searchInput sortOrder (Limit limit) (Offset offset) = do
  genres <-
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #displayName) `ilike_` val_ ("%" <> searchInput ^. #value <> "%"))
      . offset_ (fromIntegral offset)
      . limit_ (fromIntegral limit)
      . mkOrderBy sortOrder
      $ all_ ((^. #genres) wikiMusicDatabase)
  filledGenres env genres

-- | Fetch genre opinions from storage
fetchGenreOpinions' :: (MonadIO m) => Env -> [UUID] -> m (Map UUID GenreOpinion)
fetchGenreOpinions' env identifiers = do
  opinions <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList $ select $ do
      genres <-
        filter_ (\s -> (s ^. #identifier) `in_` map val_ identifiers)
          $ all_ ((^. #genres) wikiMusicDatabase)
      oneToMany_ ((^. #genreOpinions) wikiMusicDatabase) (^. #genreIdentifier) genres

  pure . Map.fromList . map toGenreOpinion $ opinions

filledGenres :: (MonadIO m) => Env -> [Genre'] -> m (Map UUID Genre, [UUID])
filledGenres env genres = do
  externalSources <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
    runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #genreIdentifier) `in_` map (val_ . (\x -> GenreId $ x ^. #identifier)) genres)
      $ all_ ((^. #genreExternalSources) wikiMusicDatabase)
  let filledGenres' = map (withExternalSources externalSources) genres
  pure (Map.fromList filledGenres', map (^. #identifier) genres)
  where
    withExternalSources externalSources genre =
      let maybeFoundExternal =
            fmap
              (fromPersistenceExternalSource . head)
              (nonEmpty $ filter (\x -> fromGenrePk (x ^. #genreIdentifier) == (genre ^. #identifier)) externalSources)
       in toGenre genre (fromMaybe emptyExternalSources maybeFoundExternal)
