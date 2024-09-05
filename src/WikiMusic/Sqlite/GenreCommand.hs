{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.GenreCommand () where

import Data.Map qualified as Map
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Sqlite
import Relude
import WikiMusic.Beam.Database
import WikiMusic.Beam.Genre
import WikiMusic.Free.GenreCommand
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Model.Artwork
import WikiMusic.Model.Comment
import WikiMusic.Model.Genre
import WikiMusic.Model.Opinion
import WikiMusic.Protolude

insertGenres' :: (MonadIO m) => Env -> [Genre] -> m (Map UUID Genre)
insertGenres' env genres = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #genres) wikiMusicDatabase)
    $ insertValues (map toPersistenceGenre genres)

  externalContents <-
    mapM
      ( \s -> do
          newIdentifier <- liftIO nextRandom
          pure $ toPersistenceGenreExternalContents s newIdentifier
      )
      genres
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #genreExternalSources) wikiMusicDatabase)
    $ insertValues externalContents
  pure Map.empty

insertGenreComments' :: (MonadIO m) => Env -> [GenreComment] -> m (Map UUID GenreComment)
insertGenreComments' env comments = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #genreComments) wikiMusicDatabase)
    $ insertValues (map mkGenreCommentP comments)
  pure Map.empty

insertGenreExternalSources' :: (MonadIO m) => Env -> [GenreExternalSources] -> m (Map UUID GenreExternalSources)
insertGenreExternalSources' env externalSources = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #genreExternalSources) wikiMusicDatabase)
    $ insertValues (map toPersistenceGenreExternalSources externalSources)
  pure Map.empty

insertGenreArtworks' :: (MonadIO m) => Env -> [GenreArtwork] -> m (Map UUID GenreArtwork)
insertGenreArtworks' env artworks = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #genreArtworks) wikiMusicDatabase)
    $ insertValues (map mkGenreArtworkP artworks)
  pure Map.empty

upsertGenreOpinions' :: (MonadIO m) => Env -> [GenreOpinion] -> m (Map UUID GenreOpinion)
upsertGenreOpinions' env opinions = do
  mapM_
    ( \o -> do
        exOpinion <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              ( \s ->
                  (s ^. #genreIdentifier)
                    ==. val_ (GenreId $ UUID.toText $ o ^. #genreIdentifier)
                    &&. (s ^. #createdBy)
                    ==. val_ (UUID.toText $ o ^. #opinion % #createdBy)
              )
              $ all_ ((^. #genreOpinions) wikiMusicDatabase)
        case exOpinion of
          Nothing ->
            liftIO
              . runBeamSqliteDebug putStrLn (env ^. #conn)
              . runInsert
              . insert ((^. #genreOpinions) wikiMusicDatabase)
              $ insertValues [mkGenreOpinionP o]
          Just oo -> do
            let newO =
                  ( oo
                      { isLike = o ^. #opinion % #isLike,
                        isDislike = o ^. #opinion % #isDislike,
                        lastEditedAt = o ^. #opinion % #lastEditedAt
                      }
                  ) ::
                    GenreOpinion'
            liftIO
              . runBeamSqliteDebug putStrLn (env ^. #conn)
              . runUpdate
              $ save ((^. #genreOpinions) wikiMusicDatabase) newO
        pure ()
    )
    opinions

  pure Map.empty

uberDeleteGenres :: (MonadIO m) => Env -> [UUID] -> m (Either GenreCommandError ())
uberDeleteGenres env identifiers = do
  deleteArtworksOfGenresResult <- liftIO . exec @GenreCommand $ deleteArtworksOfGenres env identifiers
  deleteOpinionsOfGenresResult <- liftIO . exec @GenreCommand $ deleteOpinionsOfGenres env identifiers
  deleteCommentsOfGenresResult <- liftIO . exec @GenreCommand $ deleteCommentsOfGenres env identifiers
  deleteGenreExternalSourcesResult <- liftIO . exec @GenreCommand $ deleteGenreExternalSources env identifiers
  deleteGenresResult <- doDeleteGenres' env identifiers
  pure
    $ deleteArtworksOfGenresResult
    <> deleteOpinionsOfGenresResult
    <> deleteGenreExternalSourcesResult
    <> deleteCommentsOfGenresResult
    <> deleteGenresResult

doDeleteGenres' :: (MonadIO m) => Env -> [UUID] -> m (Either GenreCommandError ())
doDeleteGenres' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #genres) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
  pure . Right $ ()

updateGenreArtworkOrder' :: (MonadIO m) => Env -> [GenreArtworkOrderUpdate] -> m (Either a ())
updateGenreArtworkOrder' env orderUpdates = do
  mapM_
    ( \ou -> do
        art <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              (\s -> (s ^. #identifier) ==. val_ (UUID.toText $ ou ^. #identifier))
              $ all_ ((^. #genreArtworks) wikiMusicDatabase)
        case art of
          Nothing -> pure ()
          Just foundArt -> do
            let a = foundArt {orderValue = fromIntegral $ ou ^. #orderValue} :: GenreArtwork'
            liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #genreArtworks) wikiMusicDatabase) $ a
    )
    orderUpdates
  pure . Right $ ()

updateGenres' :: (MonadIO m) => Env -> Map UUID (Genre, Maybe GenreDelta) -> m (Either Text ())
updateGenres' env deltas = do
  now <- liftIO getCurrentTime
  mapM_ (save'' . toPersistenceGenre . doDelta now) (Map.elems deltas)
  exUpdate <- liftIO $ exec @GenreCommand $ updateGenreExternalSources env deltas
  pure $ exUpdate <> Right ()
  where
    save'' x = liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate $ save ((^. #genres) wikiMusicDatabase) x
    doDelta :: UTCTime -> (Genre, Maybe GenreDelta) -> Genre
    doDelta now (x, xDelta') =
      case xDelta' of
        Nothing -> x
        Just xDelta ->
          x
            { displayName = fromMaybe (x ^. #displayName) (xDelta ^. #displayName),
              description = xDelta ^. #description,
              lastEditedAt = Just now
            }

updateGenreExternalSources' :: (MonadIO m) => Env -> Map UUID (Genre, Maybe GenreDelta) -> m (Either Text ())
updateGenreExternalSources' env deltas = do
  now <- liftIO getCurrentTime
  mapM_
    ( \(genre, xDelta) -> do
        ex <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              (\s -> (s ^. #genreIdentifier) ==. val_ (GenreId $ UUID.toText $ genre ^. #identifier))
              $ all_ ((^. #genreExternalSources) wikiMusicDatabase)
        case ex of
          Nothing -> pure ()
          Just foundEx -> do
            let a =
                  foundEx
                    { spotifyUrl = (^. #spotifyUrl) =<< xDelta,
                      wikipediaUrl = (^. #wikipediaUrl) =<< xDelta,
                      soundcloudUrl = (^. #soundcloudUrl) =<< xDelta,
                      youtubeUrl = (^. #youtubeUrl) =<< xDelta,
                      lastEditedAt = Just now
                    } ::
                    GenreExternalSources'
            liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #genreExternalSources) wikiMusicDatabase) $ a
    )
    deltas
  pure . Right $ ()

newGenreArtworkFromRequest' :: (MonadIO m) => UUID -> InsertGenreArtworksRequestItem -> m GenreArtwork
newGenreArtworkFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ GenreArtwork
      { genreIdentifier = req ^. #genreIdentifier,
        artwork =
          Artwork
            { identifier = newUUID,
              createdBy = createdBy,
              contentUrl = req ^. #contentUrl,
              contentCaption = req ^. #contentCaption,
              createdAt = now,
              lastEditedAt = Nothing,
              visibilityStatus = 0,
              approvedBy = Nothing,
              orderValue = req ^. #orderValue
            }
      }

newGenreOpinionFromRequest' :: (MonadIO m) => UUID -> UpsertGenreOpinionsRequestItem -> m GenreOpinion
newGenreOpinionFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ GenreOpinion
      { genreIdentifier = req ^. #genreIdentifier,
        opinion =
          Opinion
            { identifier = newUUID,
              createdBy = createdBy,
              isLike = req ^. #isLike,
              isDislike = not $ req ^. #isLike,
              createdAt = now,
              lastEditedAt = Nothing
            }
      }

newGenreFromRequest' :: (MonadIO m) => UUID -> InsertGenresRequestItem -> m Genre
newGenreFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ Genre
      { identifier = newUUID,
        parentIdentifier = Nothing,
        displayName = req ^. #displayName,
        createdBy = createdBy,
        visibilityStatus = 0,
        approvedBy = Nothing,
        createdAt = now,
        lastEditedAt = Nothing,
        artworks = fromList [],
        comments = [],
        opinions = fromList [],
        spotifyUrl = req ^. #spotifyUrl,
        youtubeUrl = req ^. #youtubeUrl,
        soundcloudUrl = req ^. #soundcloudUrl,
        wikipediaUrl = req ^. #wikipediaUrl,
        viewCount = 0,
        description = req ^. #description
      }

newGenreCommentFromRequest' :: (MonadIO m) => UUID -> InsertGenreCommentsRequestItem -> m GenreComment
newGenreCommentFromRequest' createdBy x = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ GenreComment
      { genreIdentifier = x ^. #genreIdentifier,
        comment =
          Comment
            { identifier = newUUID,
              parentIdentifier = x ^. #parentIdentifier,
              createdBy = createdBy,
              visibilityStatus = 0,
              contents = x ^. #contents,
              approvedBy = Nothing,
              createdAt = now,
              lastEditedAt = Nothing
            }
      }

deleteGenreComments' :: (MonadIO m) => Env -> [UUID] -> m (Either GenreCommandError ())
deleteGenreComments' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #genreComments) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
  pure . Right $ ()

deleteGenreArtworks' :: (MonadIO m) => Env -> [UUID] -> m (Either GenreCommandError ())
deleteGenreArtworks' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #genreArtworks) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
  pure . Right $ ()

deleteGenreOpinions' :: (MonadIO m) => Env -> [UUID] -> m (Either GenreCommandError ())
deleteGenreOpinions' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #genreOpinions) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map (val_ . UUID.toText) identifiers)
  pure . Right $ ()

deleteCommentsOfGenres' :: (MonadIO m) => Env -> [UUID] -> m (Either GenreCommandError ())
deleteCommentsOfGenres' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #genreComments) wikiMusicDatabase)
      (\c -> (c ^. #genreIdentifier) `in_` map (val_ . GenreId . UUID.toText) identifiers)
  pure . Right $ ()

incrementViewsByOne' :: (MonadIO m) => Env -> [UUID] -> m (Either GenreCommandError ())
incrementViewsByOne' env identifiers = do
  mapM_ doUpdate identifiers
  pure $ Right ()
  where
    doUpdate x = do
      ex <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
        runSelectReturningOne $ select $ do
          filter_
            (\s -> (s ^. #identifier) ==. (val_ . UUID.toText $ x))
            $ all_ ((^. #genres) wikiMusicDatabase)
      case ex of
        Nothing -> pure ()
        Just foundEx -> do
          let a =
                foundEx
                  { viewCount = (foundEx ^. #viewCount) + 1
                  } ::
                  Genre'
          liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #genres) wikiMusicDatabase) $ a

instance Exec GenreCommand where
  execAlgebra (IncrementViewsByOne env identifiers next) =
    next =<< incrementViewsByOne' env identifiers
  execAlgebra (InsertGenres env genres next) =
    next =<< insertGenres' env genres
  execAlgebra (InsertGenreComments env comments next) =
    next =<< insertGenreComments' env comments
  execAlgebra (InsertGenreExternalSources env externalSources next) =
    next =<< insertGenreExternalSources' env externalSources
  execAlgebra (InsertGenreArtworks env artworks next) =
    next =<< insertGenreArtworks' env artworks
  execAlgebra (UpsertGenreOpinions env opinions next) =
    next =<< upsertGenreOpinions' env opinions
  execAlgebra (DeleteGenres env identifiers next) =
    next =<< uberDeleteGenres env identifiers
  execAlgebra (DeleteGenreComments env identifiers next) =
    next =<< deleteGenreComments' env identifiers
  execAlgebra (DeleteGenreArtworks env identifiers next) =
    next =<< deleteGenreArtworks' env identifiers
  execAlgebra (DeleteGenreOpinions env identifiers next) =
    next =<< deleteGenreOpinions' env identifiers
  execAlgebra (DeleteCommentsOfGenres env identifiers next) = do
    let ids = map UUID.toText identifiers

    mapM_
      ( \y ->
          runBeamSqliteDebug putStrLn (env ^. #conn)
            . runDelete
            $ delete ((^. #genreComments) wikiMusicDatabase) (\c -> c ^. #genreIdentifier ==. (val_ . GenreId $ y))
      )
      ids

    next $ Right ()
  execAlgebra (DeleteGenreExternalSources env identifiers next) = do
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runDelete
      $ delete
        ((^. #genreExternalSources) wikiMusicDatabase)
        (\c -> (c ^. #genreIdentifier) `in_` map (val_ . GenreId . UUID.toText) identifiers)
    next . Right $ ()
  execAlgebra (DeleteArtworksOfGenres env identifiers next) = do
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runDelete
      $ delete
        ((^. #genreArtworks) wikiMusicDatabase)
        (\c -> (c ^. #genreIdentifier) `in_` map (val_ . GenreId . UUID.toText) identifiers)
    next . Right $ ()
  execAlgebra (DeleteOpinionsOfGenres env identifiers next) = do
    liftIO
      . runBeamSqliteDebug putStrLn (env ^. #conn)
      . runDelete
      $ delete
        ((^. #genreOpinions) wikiMusicDatabase)
        (\c -> (c ^. #genreIdentifier) `in_` map (val_ . GenreId . UUID.toText) identifiers)
    next . Right $ ()
  execAlgebra (UpdateGenreArtworkOrder env orderUpdates next) =
    next =<< updateGenreArtworkOrder' env orderUpdates
  execAlgebra (UpdateGenres env deltas next) =
    next =<< updateGenres' env deltas
  execAlgebra (UpdateGenreExternalSources env deltas next) =
    next =<< updateGenreExternalSources' env deltas
  execAlgebra (NewGenreFromRequest createdBy req next) =
    next =<< newGenreFromRequest' createdBy req
  execAlgebra (NewGenreCommentFromRequest createdBy req next) =
    next =<< newGenreCommentFromRequest' createdBy req
  execAlgebra (NewGenreOpinionFromRequest createdBy req next) =
    next =<< newGenreOpinionFromRequest' createdBy req
  execAlgebra (NewGenreArtworkFromRequest createdBy req next) =
    next =<< newGenreArtworkFromRequest' createdBy req
