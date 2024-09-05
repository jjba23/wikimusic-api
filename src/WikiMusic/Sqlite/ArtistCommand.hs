{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Sqlite.ArtistCommand () where

import Data.Map qualified as Map
import Data.Text (pack)
import Database.Beam
import Database.Beam.Sqlite
import Relude
import WikiMusic.Beam.Artist
import WikiMusic.Beam.Database
import WikiMusic.Free.ArtistCommand
import WikiMusic.Interaction.Model.Artist
import WikiMusic.Model.Artist
import WikiMusic.Model.Artwork
import WikiMusic.Model.Comment
import WikiMusic.Model.Opinion
import WikiMusic.Protolude

insertArtists' :: (MonadIO m) => Env -> [Artist] -> m ()
insertArtists' env artists = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #artists) wikiMusicDatabase)
    $ insertValues (map toPersistenceArtist artists)

  externalContents <-
    mapM
      ( \s -> do
          newIdentifier <- liftIO nextRandom
          pure $ toPersistenceArtistExternalContents s newIdentifier
      )
      artists
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #artistExternalSources) wikiMusicDatabase)
    $ insertValues externalContents

insertArtistComments' :: (MonadIO m) => Env -> [ArtistComment] -> m ()
insertArtistComments' env comments = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #artistComments) wikiMusicDatabase)
    $ insertValues (map toPersistenceArtistComment comments)

insertArtistExternalSources' :: (MonadIO m) => Env -> [ArtistExternalSources] -> m (Map UUID ArtistExternalSources)
insertArtistExternalSources' env externalSources = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #artistExternalSources) wikiMusicDatabase)
    $ insertValues (map mkArtistExSourcesP externalSources)
  pure Map.empty

insertArtistArtworks' :: (MonadIO m) => Env -> [ArtistArtwork] -> m (Map UUID ArtistArtwork)
insertArtistArtworks' env artworks = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #artistArtworks) wikiMusicDatabase)
    $ insertValues (map mkArtistArtworkP artworks)
  pure Map.empty

upsertArtistOpinions' :: (MonadIO m) => Env -> [ArtistOpinion] -> m (Map UUID ArtistOpinion)
upsertArtistOpinions' env opinions = do
  mapM_
    ( \o -> do
        exOpinion <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              ( \s ->
                  (s ^. #artistIdentifier)
                    ==. val_ (ArtistId $ o ^. #artistIdentifier)
                    &&. (s ^. #createdBy)
                    ==. val_ (o ^. #opinion % #createdBy)
              )
              $ all_ ((^. #artistOpinions) wikiMusicDatabase)
        case exOpinion of
          Nothing ->
            liftIO
              . runBeamSqliteDebug putStrLn (env ^. #conn)
              . runInsert
              . insert ((^. #artistOpinions) wikiMusicDatabase)
              $ insertValues [mkArtistOpinionP o]
          Just oo -> do
            let newO =
                  ( oo
                      { isLike = o ^. #opinion % #isLike,
                        isDislike = o ^. #opinion % #isDislike,
                        lastEditedAt = o ^. #opinion % #lastEditedAt
                      }
                  ) ::
                    ArtistOpinion'
            liftIO
              . runBeamSqliteDebug putStrLn (env ^. #conn)
              . runUpdate
              $ save ((^. #artistOpinions) wikiMusicDatabase) newO
        pure ()
    )
    opinions

  pure Map.empty

deleteArtists' :: (MonadIO m) => Env -> [UUID] -> m (Either ArtistCommandError ())
deleteArtists' env identifiers = do
  deleteArtworksOfArtistsResult <- liftIO . exec @ArtistCommand $ deleteArtworksOfArtists env identifiers
  deleteOpinionsOfArtistsResult <- liftIO . exec @ArtistCommand $ deleteOpinionsOfArtists env identifiers
  deleteCommentsOfArtistsResult <- liftIO . exec @ArtistCommand $ deleteCommentsOfArtists env identifiers
  deleteArtistExternalSourcesResult <- liftIO . exec @ArtistCommand $ deleteArtistExternalSources env identifiers
  deleteArtistsResult <- deleteStuffByUUID (env ^. #pool) "artists" "identifier" identifiers
  pure
    $ deleteArtworksOfArtistsResult
    <> deleteOpinionsOfArtistsResult
    <> deleteArtistExternalSourcesResult
    <> deleteCommentsOfArtistsResult
    <> first fromHasqlUsageError deleteArtistsResult

updateArtistArtworkOrder' :: (MonadIO m) => Env -> [ArtistArtworkOrderUpdate] -> m (Either a ())
updateArtistArtworkOrder' env orderUpdates = do
  mapM_
    ( \ou -> do
        art <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              (\s -> (s ^. #identifier) ==. val_ (ou ^. #identifier))
              $ all_ ((^. #artistArtworks) wikiMusicDatabase)
        case art of
          Nothing -> pure ()
          Just foundArt -> do
            let a = foundArt {orderValue = fromIntegral $ ou ^. #orderValue} :: ArtistArtwork'
            liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #artistArtworks) wikiMusicDatabase) $ a
    )
    orderUpdates
  pure . Right $ ()

updateArtists' :: (MonadIO m) => Env -> Map UUID (Artist, Maybe ArtistDelta) -> m (Either Text ())
updateArtists' env deltas = do
  now <- liftIO getCurrentTime
  mapM_ (save'' . toPersistenceArtist . doDelta now) (Map.elems deltas)
  exUpdate <- liftIO $ exec @ArtistCommand $ updateArtistExternalSources env deltas
  pure $ exUpdate <> Right ()
  where
    save'' x = liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate $ save ((^. #artists) wikiMusicDatabase) x
    doDelta now (x, xDelta') =
      case xDelta' of
        Nothing -> x
        Just xDelta ->
          x
            { displayName = fromMaybe (x ^. #displayName) (xDelta ^. #displayName),
              description = xDelta ^. #description,
              lastEditedAt = Just now
            } ::
            Artist

updateArtistExternalSources' :: (MonadIO m) => Env -> Map UUID (Artist, Maybe ArtistDelta) -> m (Either Text ())
updateArtistExternalSources' env deltas = do
  now <- liftIO getCurrentTime
  mapM_
    ( \(artist, xDelta) -> do
        ex <- liftIO $ runBeamSqliteDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              (\s -> (s ^. #artistIdentifier) ==. val_ (ArtistId $ artist ^. #identifier))
              $ all_ ((^. #artistExternalSources) wikiMusicDatabase)
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
                    ArtistExternalSources'
            liftIO . runBeamSqliteDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #artistExternalSources) wikiMusicDatabase) $ a
    )
    deltas
  pure . Right $ ()

newArtistArtworkFromRequest' :: (MonadIO m) => UUID -> InsertArtistArtworksRequestItem -> m ArtistArtwork
newArtistArtworkFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ ArtistArtwork
      { artistIdentifier = req ^. #artistIdentifier,
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

newArtistOpinionFromRequest' :: (MonadIO m) => UUID -> UpsertArtistOpinionsRequestItem -> m ArtistOpinion
newArtistOpinionFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ ArtistOpinion
      { artistIdentifier = req ^. #artistIdentifier,
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

newArtistFromRequest' :: (MonadIO m) => UUID -> InsertArtistsRequestItem -> m Artist
newArtistFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ Artist
      { identifier = newUUID,
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

newArtistCommentFromRequest' :: (MonadIO m) => UUID -> InsertArtistCommentsRequestItem -> m ArtistComment
newArtistCommentFromRequest' createdBy x = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ ArtistComment
      { artistIdentifier = x ^. #artistIdentifier,
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

deleteArtistComments' :: (MonadIO m) => Env -> [UUID] -> m (Either ArtistCommandError ())
deleteArtistComments' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #artistComments) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map val_ identifiers)
  pure . Right $ ()

deleteArtistArtworks' :: (MonadIO m) => Env -> [UUID] -> m (Either ArtistCommandError ())
deleteArtistArtworks' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #artistArtworks) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map val_ identifiers)
  pure . Right $ ()

deleteArtistOpinions' :: (MonadIO m) => Env -> [UUID] -> m (Either ArtistCommandError ())
deleteArtistOpinions' env identifiers = do
  liftIO
    . runBeamSqliteDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #artistOpinions) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map val_ identifiers)
  pure . Right $ ()

instance Exec ArtistCommand where
  execAlgebra (IncrementViewsByOne env identifiers next) =
    next =<< incrementViewsByOne' env identifiers "artists"
  execAlgebra (InsertArtists env artists next) =
    next =<< insertArtists' env artists
  execAlgebra (InsertArtistComments env comments next) =
    next =<< insertArtistComments' env comments
  execAlgebra (InsertArtistExternalSources env externalSources next) =
    next =<< insertArtistExternalSources' env externalSources
  execAlgebra (InsertArtistArtworks env artworks next) =
    next =<< insertArtistArtworks' env artworks
  execAlgebra (UpsertArtistOpinions env opinions next) =
    next =<< upsertArtistOpinions' env opinions
  execAlgebra (DeleteArtists env identifiers next) =
    next =<< deleteArtists' env identifiers
  execAlgebra (DeleteArtistComments env identifiers next) = do
    next =<< deleteArtistComments' env identifiers
  execAlgebra (DeleteArtistArtworks env identifiers next) = do
    next =<< deleteArtistArtworks' env identifiers
  execAlgebra (DeleteArtistOpinions env identifiers next) = do
    next =<< deleteArtistOpinions' env identifiers
  execAlgebra (DeleteCommentsOfArtists env identifiers next) = do
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "artist_comments" "artist_identifier" identifiers
  execAlgebra (DeleteArtistExternalSources env identifiers next) = do
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "artist_external_sources" "artist_identifier" identifiers
  execAlgebra (DeleteArtworksOfArtists env identifiers next) = do
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "artist_artworks" "artist_identifier" identifiers
  execAlgebra (DeleteOpinionsOfArtists env identifiers next) = do
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "artist_opinions" "artist_identifier" identifiers
  execAlgebra (UpdateArtistArtworkOrder env orderUpdates next) =
    next =<< updateArtistArtworkOrder' env orderUpdates
  execAlgebra (UpdateArtists env deltas next) =
    next =<< updateArtists' env deltas
  execAlgebra (UpdateArtistExternalSources env deltas next) =
    next =<< updateArtistExternalSources' env deltas
  execAlgebra (NewArtistFromRequest createdBy req next) =
    next =<< newArtistFromRequest' createdBy req
  execAlgebra (NewArtistCommentFromRequest createdBy req next) =
    next =<< newArtistCommentFromRequest' createdBy req
  execAlgebra (NewArtistOpinionFromRequest createdBy req next) =
    next =<< newArtistOpinionFromRequest' createdBy req
  execAlgebra (NewArtistArtworkFromRequest createdBy req next) =
    next =<< newArtistArtworkFromRequest' createdBy req
