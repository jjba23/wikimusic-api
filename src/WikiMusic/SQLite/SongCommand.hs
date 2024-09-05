{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SQLite.SongCommand () where

import Data.Map qualified as Map
import Data.Text (pack)
import Database.Beam
import Database.Beam.Postgres
import Hasql.Decoders as D
import Hasql.Encoders as E
import Hasql.Pool qualified
import Hasql.Statement (Statement (..))
import Relude
import WikiMusic.Beam.Database
import WikiMusic.Beam.Relations
import WikiMusic.Beam.Song
import WikiMusic.Free.SongCommand
import WikiMusic.Interaction.Model.Song
import WikiMusic.Model.Artwork
import WikiMusic.Model.Comment
import WikiMusic.Model.Opinion
import WikiMusic.Model.Song
import WikiMusic.Protolude
import Prelude qualified

insertArtistsOfSongs' :: (MonadIO m) => Env -> [ArtistOfSong] -> m (Map UUID ArtistOfSong)
insertArtistsOfSongs' env items = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #songArtists) wikiMusicDatabase)
    $ insertValues (map mkSongArtistP items)
  pure Map.empty

insertSongs' :: (MonadIO m) => Env -> [Song] -> m (Map UUID Song)
insertSongs' env songs = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #songs) wikiMusicDatabase)
    $ insertValues (map toPersistenceSong songs)

  externalContents <-
    mapM
      ( \s -> do
          newIdentifier <- liftIO nextRandom
          pure $ toPersistenceSongExternalContents s newIdentifier
      )
      songs
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #songExternalSources) wikiMusicDatabase)
    $ insertValues externalContents
  pure Map.empty

insertSongComments' :: (MonadIO m) => Env -> [SongComment] -> m (Either SongCommandError ())
insertSongComments' env comments = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #songComments) wikiMusicDatabase)
    $ insertValues (map toPersistenceSongComment comments)
  pure . Right $ ()

insertSongExternalSources' :: (MonadIO m) => Env -> [SongExternalSources] -> m (Map UUID SongExternalSources)
insertSongExternalSources' env externalSources = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #songExternalSources) wikiMusicDatabase)
    $ insertValues (map toPersistenceSongExternalSources externalSources)
  pure Map.empty

insertSongArtworks' :: (MonadIO m) => Env -> [SongArtwork] -> m (Map UUID SongArtwork)
insertSongArtworks' env artworks = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #songArtworks) wikiMusicDatabase)
    $ insertValues (map mkSongArtworkP artworks)
  pure Map.empty

upsertSongOpinions' :: (MonadIO m) => Env -> [SongOpinion] -> m (Map UUID SongOpinion)
upsertSongOpinions' env opinions = do
  mapM_
    ( \o -> do
        exOpinion <- liftIO $ runBeamPostgresDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              ( \s ->
                  (s ^. #songIdentifier)
                    ==. val_ (SongId $ o ^. #songIdentifier)
                    &&. (s ^. #createdBy)
                    ==. val_ (o ^. #opinion % #createdBy)
              )
              $ all_ ((^. #songOpinions) wikiMusicDatabase)
        case exOpinion of
          Nothing ->
            liftIO
              . runBeamPostgresDebug putStrLn (env ^. #conn)
              . runInsert
              . insert ((^. #songOpinions) wikiMusicDatabase)
              $ insertValues [mkSongOpinionP o]
          Just oo -> do
            let newO =
                  ( oo
                      { isLike = o ^. #opinion % #isLike,
                        isDislike = o ^. #opinion % #isDislike,
                        lastEditedAt = o ^. #opinion % #lastEditedAt
                      }
                  ) ::
                    SongOpinion'
            liftIO
              . runBeamPostgresDebug putStrLn (env ^. #conn)
              . runUpdate
              $ save ((^. #songOpinions) wikiMusicDatabase) newO
        pure ()
    )
    opinions

  pure Map.empty

uberDeleteSongs' :: (MonadIO m) => Env -> [UUID] -> m (Either SongCommandError ())
uberDeleteSongs' env identifiers = do
  deleteArtworksOfSongsResult <- liftIO . exec @SongCommand $ deleteArtworksOfSongs env identifiers
  deleteOpinionsOfSongsResult <- liftIO . exec @SongCommand $ deleteOpinionsOfSongs env identifiers
  deleteCommentsOfSongsResult <- liftIO . exec @SongCommand $ deleteCommentsOfSongs env identifiers
  deleteSongExternalSourcesResult <- liftIO . exec @SongCommand $ deleteSongExternalSources env identifiers
  deleteSongsResult <- deleteStuffByUUID (env ^. #pool) "songs" "identifier" identifiers
  deleteArtistsOfSongResult <- liftIO . exec @SongCommand $ deleteArtistsOfSongs env identifiers
  deleteContentsOfSongResult <- liftIO . exec @SongCommand $ deleteContentsOfSongs env identifiers
  pure
    $ deleteArtworksOfSongsResult
    <> deleteOpinionsOfSongsResult
    <> deleteSongExternalSourcesResult
    <> deleteCommentsOfSongsResult
    <> deleteArtistsOfSongResult
    <> deleteContentsOfSongResult
    <> first fromHasqlUsageError deleteSongsResult

updateSongArtworkOrder' :: (MonadIO m) => Env -> [SongArtworkOrderUpdate] -> m (Either a ())
updateSongArtworkOrder' env orderUpdates = do
  mapM_
    ( \ou -> do
        art <- liftIO $ runBeamPostgresDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              (\s -> (s ^. #identifier) ==. val_ (ou ^. #identifier))
              $ all_ ((^. #songArtworks) wikiMusicDatabase)
        case art of
          Nothing -> pure ()
          Just foundArt -> do
            let a = foundArt {orderValue = fromIntegral $ ou ^. #orderValue} :: SongArtwork'
            liftIO . runBeamPostgresDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #songArtworks) wikiMusicDatabase) $ a
    )
    orderUpdates
  pure . Right $ ()

updateSongs' :: (MonadIO m) => Env -> Map UUID (Song, Maybe SongDelta) -> m (Either Text ())
updateSongs' env deltas = do
  now <- liftIO getCurrentTime
  mapM_ (save'' . toPersistenceSong . doDelta now) (Map.elems deltas)
  exUpdate <- liftIO $ exec @SongCommand $ updateSongExternalSources env deltas
  pure $ exUpdate <> Right ()
  where
    save'' x =
      liftIO
        . runBeamPostgresDebug putStrLn (env ^. #conn)
        . runUpdate
        $ save ((^. #songs) wikiMusicDatabase) x
    doDelta :: UTCTime -> (Song, Maybe SongDelta) -> Song
    doDelta now (x, xDelta') =
      case xDelta' of
        Nothing -> x
        Just xDelta ->
          x
            { musicKey = xDelta ^. #musicKey,
              musicTuning = xDelta ^. #musicTuning,
              musicCreationDate = xDelta ^. #musicCreationDate,
              albumName = xDelta ^. #albumName,
              albumInfoLink = xDelta ^. #albumInfoLink,
              displayName = fromMaybe (x ^. #displayName) (xDelta ^. #displayName),
              description = xDelta ^. #description,
              lastEditedAt = Just now
            }

updateSongExternalSources' :: (MonadIO m) => Env -> Map UUID (Song, Maybe SongDelta) -> m (Either Text ())
updateSongExternalSources' env deltas = do
  now <- liftIO getCurrentTime
  mapM_
    ( \(song, xDelta) -> do
        ex <- liftIO $ runBeamPostgresDebug putStrLn (env ^. #conn) $ do
          runSelectReturningOne $ select $ do
            filter_
              (\s -> (s ^. #songIdentifier) ==. val_ (SongId $ song ^. #identifier))
              $ all_ ((^. #songExternalSources) wikiMusicDatabase)
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
                    SongExternalSources'
            liftIO . runBeamPostgresDebug putStrLn (env ^. #conn) . runUpdate . save ((^. #songExternalSources) wikiMusicDatabase) $ a
    )
    deltas
  pure . Right $ ()

newSongArtworkFromRequest' :: (MonadIO m) => UUID -> InsertSongArtworksRequestItem -> m SongArtwork
newSongArtworkFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ SongArtwork
      { songIdentifier = req ^. #songIdentifier,
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

newSongOpinionFromRequest' :: (MonadIO m) => UUID -> UpsertSongOpinionsRequestItem -> m SongOpinion
newSongOpinionFromRequest' createdBy req = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ SongOpinion
      { songIdentifier = req ^. #songIdentifier,
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

updateSongContents' :: (MonadIO m) => Env -> [SongContentDelta] -> m (Either Text ())
updateSongContents' env songContentDeltas = do
  songContentsP <-
    liftIO
      . runBeamPostgresDebug putStrLn (env ^. #conn)
      . runSelectReturningList
      . select
      . filter_ (\s -> (s ^. #identifier) `in_` map (val_ . (\z -> z ^. #identifier)) songContentDeltas)
      $ all_ ((^. #songContents) wikiMusicDatabase)
  _ <- liftIO . putStrLn . Relude.show $ songContentDeltas
  now <- liftIO getCurrentTime
  let updatedP =
        map
          ( \songContentDelta -> do
              fmap (updatedSongContent songContentDelta now . head)
                $ nonEmpty
                $ filter
                  ( \songContentP ->
                      songContentP
                        ^. #identifier
                        == songContentDelta
                        ^. #identifier
                  )
                  songContentsP
          )
          songContentDeltas
  mapM_
    ( liftIO
        . runBeamPostgresDebug putStrLn (env ^. #conn)
        . runUpdate
        . save ((^. #songContents) wikiMusicDatabase)
    )
    (catMaybes updatedP)
  pure . Right $ ()

updatedSongContent :: SongContentDelta -> UTCTime -> SongContents' -> SongContents'
updatedSongContent songContentDelta now x =
  x
    { versionName = songContentDelta ^. #versionName,
      instrumentType = fromMaybe "" (songContentDelta ^. #instrumentType),
      asciiLegend = songContentDelta ^. #asciiLegend,
      asciiContents = songContentDelta ^. #asciiContents,
      pdfContents = songContentDelta ^. #pdfContents,
      guitarProContents = songContentDelta ^. #guitarProContents,
      lastEditedAt = Just now
    } ::
    SongContents'

deleteArtistOfSong' :: (MonadIO m) => Env -> (UUID, UUID) -> m (Either SongCommandError ())
deleteArtistOfSong' env identifiers = do
  stmtResult <- hasqlTransaction (env ^. #pool) stmt identifiers
  pure $ first fromHasqlUsageError stmtResult
  where
    query =
      encodeUtf8
        [trimming|
    DELETE FROM song_artists WHERE song_identifier = $$1 AND artist_identifier = $$2
    |]
    encoder =
      contrazip2
        (E.param . E.nonNullable $ E.uuid)
        (E.param . E.nonNullable $ E.uuid)
    stmt = Statement query encoder D.noResult True

newSongCommentFromRequest' :: (MonadIO m) => UUID -> InsertSongCommentsRequestItem -> m SongComment
newSongCommentFromRequest' createdBy x = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ SongComment
      { songIdentifier = x ^. #songIdentifier,
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

insertSongContents' :: (MonadIO m) => Env -> [SongContent] -> m (Map UUID SongContent)
insertSongContents' env contents = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runInsert
    . insert ((^. #songContents) wikiMusicDatabase)
    $ insertValues (map mkSongContentsP contents)
  pure Map.empty

newSongFromRequest' :: (MonadIO m) => UUID -> InsertSongsRequestItem -> m Song
newSongFromRequest' createdBy song = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ Song
      { identifier = newUUID,
        displayName = song ^. #displayName,
        musicKey = song ^. #musicKey,
        musicTuning = song ^. #musicTuning,
        musicCreationDate = song ^. #musicCreationDate,
        albumName = song ^. #albumName,
        albumInfoLink = song ^. #albumInfoLink,
        createdBy = createdBy,
        visibilityStatus = 0,
        approvedBy = Nothing,
        createdAt = now,
        lastEditedAt = Nothing,
        artworks = Map.empty,
        comments = [],
        opinions = Map.empty,
        contents = Map.empty,
        spotifyUrl = song ^. #spotifyUrl,
        youtubeUrl = song ^. #youtubeUrl,
        soundcloudUrl = song ^. #soundcloudUrl,
        wikipediaUrl = song ^. #wikipediaUrl,
        artists = Map.empty,
        viewCount = 0,
        description = song ^. #description
      }

newArtistOfSongFromRequest' :: (MonadIO m) => UUID -> InsertArtistsOfSongsRequestItem -> m ArtistOfSong
newArtistOfSongFromRequest' createdBy x = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ ArtistOfSong
      { identifier = newUUID,
        songIdentifier = x ^. #songIdentifier,
        artistIdentifier = x ^. #artistIdentifier,
        createdAt = now,
        createdBy = createdBy
      }

newSongContentFromRequest' :: (MonadIO m) => UUID -> InsertSongContentsRequestItem -> m SongContent
newSongContentFromRequest' createdBy x = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  pure
    $ SongContent
      { identifier = newUUID,
        songIdentifier = x ^. #songIdentifier,
        versionName = x ^. #versionName,
        visibilityStatus = 0,
        approvedBy = Nothing,
        instrumentType = x ^. #instrumentType,
        asciiLegend = x ^. #asciiLegend,
        asciiContents = x ^. #asciiContents,
        pdfContents = x ^. #pdfContents,
        guitarProContents = x ^. #guitarProContents,
        createdAt = now,
        createdBy = createdBy,
        lastEditedAt = Nothing
      }

deleteSongComments' :: (MonadIO m) => Env -> [UUID] -> m (Either SongCommandError ())
deleteSongComments' env identifiers = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #songComments) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map val_ identifiers)
  pure . Right $ ()

deleteSongArtworks' :: (MonadIO m) => Env -> [UUID] -> m (Either SongCommandError ())
deleteSongArtworks' env identifiers = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #songArtworks) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map val_ identifiers)
  pure . Right $ ()

deleteSongOpinions' :: (MonadIO m) => Env -> [UUID] -> m (Either SongCommandError ())
deleteSongOpinions' env identifiers = do
  liftIO
    . runBeamPostgresDebug putStrLn (env ^. #conn)
    . runDelete
    $ delete
      ((^. #songOpinions) wikiMusicDatabase)
      (\c -> (c ^. #identifier) `in_` map val_ identifiers)
  pure . Right $ ()

instance Exec SongCommand where
  execAlgebra (IncrementViewsByOne env identifiers next) =
    next =<< incrementViewsByOne' env identifiers "songs"
  execAlgebra (InsertSongs env songs next) =
    next =<< insertSongs' env songs
  execAlgebra (InsertSongComments env comments next) =
    next =<< insertSongComments' env comments
  execAlgebra (InsertSongExternalSources env externalSources next) =
    next =<< insertSongExternalSources' env externalSources
  execAlgebra (InsertSongArtworks env artworks next) =
    next =<< insertSongArtworks' env artworks
  execAlgebra (UpsertSongOpinions env opinions next) =
    next =<< upsertSongOpinions' env opinions
  execAlgebra (DeleteSongs env identifiers next) =
    next =<< uberDeleteSongs' env identifiers
  execAlgebra (DeleteSongComments env identifiers next) = do
    next =<< deleteSongComments' env identifiers
  execAlgebra (DeleteSongArtworks env identifiers next) = do
    next =<< deleteSongArtworks' env identifiers
  execAlgebra (DeleteSongOpinions env identifiers next) = do
    next =<< deleteSongOpinions' env identifiers
  execAlgebra (DeleteCommentsOfSongs env identifiers next) =
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "song_comments" "song_identifier" identifiers
  execAlgebra (DeleteSongExternalSources env identifiers next) =
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "song_external_sources" "song_identifier" identifiers
  execAlgebra (DeleteArtworksOfSongs env identifiers next) =
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "song_artworks" "song_identifier" identifiers
  execAlgebra (DeleteOpinionsOfSongs env identifiers next) =
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "song_opinions" "song_identifier" identifiers
  execAlgebra (UpdateSongArtworkOrder env orderUpdates next) =
    next =<< updateSongArtworkOrder' env orderUpdates
  execAlgebra (UpdateSongs env deltas next) =
    next =<< updateSongs' env deltas
  execAlgebra (UpdateSongContents env deltas next) =
    next =<< updateSongContents' env deltas
  execAlgebra (UpdateSongExternalSources env deltas next) =
    next =<< updateSongExternalSources' env deltas
  execAlgebra (NewSongCommentFromRequest createdBy req next) =
    next =<< newSongCommentFromRequest' createdBy req
  execAlgebra (NewSongOpinionFromRequest createdBy req next) =
    next =<< newSongOpinionFromRequest' createdBy req
  execAlgebra (NewSongArtworkFromRequest createdBy req next) =
    next =<< newSongArtworkFromRequest' createdBy req
  execAlgebra (InsertSongContents env contents next) =
    next =<< insertSongContents' env contents
  execAlgebra (InsertArtistsOfSongs env items next) =
    next =<< insertArtistsOfSongs' env items
  execAlgebra (DeleteArtistsOfSongs env identifiers next) =
    next . first fromHasqlUsageError =<< deleteStuffByUUID (env ^. #pool) "song_artists" "song_identifier" identifiers
  execAlgebra (DeleteArtistOfSong env identifiers next) =
    next =<< deleteArtistOfSong' env identifiers
  execAlgebra (NewSongFromRequest createdBy song next) =
    next =<< newSongFromRequest' createdBy song
  execAlgebra (NewArtistOfSongFromRequest createdBy x next) =
    next =<< newArtistOfSongFromRequest' createdBy x
  execAlgebra (NewSongContentFromRequest createdBy x next) =
    next =<< newSongContentFromRequest' createdBy x
  execAlgebra (DeleteContentsOfSongs env identifiers next) = do
    stmtResult <- deleteStuffByUUID (env ^. #pool) "song_contents" "song_identifier" identifiers
    next $ first fromHasqlUsageError stmtResult
  execAlgebra (DeleteSongContents env identifiers next) = do
    stmtResult <- deleteStuffByUUID (env ^. #pool) "song_contents" "identifier" identifiers
    next $ first fromHasqlUsageError stmtResult

