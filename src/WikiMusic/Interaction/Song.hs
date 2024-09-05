{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.Song
  ( fetchSongsAction,
    insertSongsAction,
    insertSongCommentsAction,
    insertSongArtworksAction,
    upsertSongOpinionsAction,
    deleteSongsByIdentifierAction,
    deleteSongCommentsByIdentifierAction,
    deleteSongOpinionsByIdentifierAction,
    deleteSongArtworksByIdentifierAction,
    updateSongArtworksOrderAction,
    updateSongAction,
    insertArtistsOfSongAction,
    fetchSongAction,
    updateSongContentsAction,
    deleteSongContentsByIdentifierAction,
    insertSongContentsAction,
    searchSongsAction,
    deleteArtistsOfSongAction,
  )
where

import Data.Map qualified as Map
import Data.Text (pack, take, unpack)
import Relude
import WikiMusic.Free.Logger
import WikiMusic.Free.SongCommand
import WikiMusic.Free.SongQuery
import WikiMusic.Interaction.Model.Song
import WikiMusic.Model.Other
import WikiMusic.Model.Song
import WikiMusic.Protolude

fetchSongsAction ::
  (SongQuery :<: f, SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Text ->
  Free f (Either SongError GetSongsQueryResponse)
fetchSongsAction env authUser limit offset maybeSortOrder maybeInclude =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (songMap, sortOrderList) <- fetchSongs env sortOrder limit offset

    enrichedSongs <-
      enrichedSongResponse
        env
        songMap
        (maybe noEnrichment parseInclude maybeInclude)

    pure . Right $ GetSongsQueryResponse {songs = enrichedSongs, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

searchSongsAction ::
  (SongQuery :<: f, SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  Free f (Either SongError GetSongsQueryResponse)
searchSongsAction env authUser limit offset maybeSortOrder maybeInclude searchInput =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (songMap, sortOrderList) <- searchSongs env (SearchInput searchInput) sortOrder limit offset

    enrichedSongs <-
      enrichedSongResponse
        env
        songMap
        (maybe noEnrichment parseInclude maybeInclude)

    pure . Right $ GetSongsQueryResponse {songs = enrichedSongs, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

fetchSongAction ::
  (SongQuery :<: f, SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Maybe Text ->
  Maybe Text ->
  Free f (Either SongError GetSongsQueryResponse)
fetchSongAction env authUser identifier maybeSortOrder maybeInclude =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (songMap, sortOrderList) <- fetchSongsByUUID env sortOrder [identifier]

    enrichedSongs <-
      enrichedSongResponse
        env
        songMap
        (maybe noEnrichment parseInclude maybeInclude)

    _ <- incrementViewsByOne env (Map.keys songMap)

    pure . Right $ GetSongsQueryResponse {songs = enrichedSongs, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

insertSongsAction ::
  (SongQuery :<: f, SongCommand :<: f, Logger :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertSongsRequest ->
  Free f (Either SongError InsertSongsCommandResponse)
insertSongsAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    newSongs <- mapM (newSongFromRequest (authUser ^. #identifier)) (request ^. #songs)

    let entityValidation x = (x ^. #displayName, validateSong x)
        validationResults = fromList . map (first (pack . Relude.show) . entityValidation) $ newSongs
        newSongIdentifiers = map (^. #identifier) newSongs

    ifAllValid validationResults $ do
      _ <- insertSongs env newSongs
      _ <- logInfo "INSERTING NEW SONGS:"
      _ <- logInfo . pack . Relude.show $ newSongs
      (songMap, sortOrder) <- fetchSongsByUUID env DescCreatedAt newSongIdentifiers

      enrichedInsertedSongs <- enrichedSongResponse env songMap fullEnrichment
      pure
        . Right
        $ InsertSongsQueryResponse
          { songs = enrichedInsertedSongs,
            sortOrder = sortOrder,
            validationResults = validationResults
          }

insertSongCommentsAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertSongCommentsRequest ->
  Free f (Either SongError InsertSongCommentsCommandResponse)
insertSongCommentsAction env authUser request =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    newComments <- mapM (newSongCommentFromRequest (authUser ^. #identifier)) (request ^. #songComments)
    let entityValidation x = (Data.Text.take 20 (x ^. #comment % #contents), validateSongComment x)
        validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newComments

    ifAllValid validationResults $ do
      _ <- insertSongComments env newComments
      pure
        . Right
        $ InsertSongCommentsCommandResponse
          { songComments = Map.empty,
            validationResults = validationResults
          }

upsertSongOpinionsAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UpsertSongOpinionsRequest ->
  Free f (Either SongError UpsertSongOpinionsCommandResponse)
upsertSongOpinionsAction env authUser request =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    let entityValidation x = (x ^. #songIdentifier, validateSongOpinion x)

    newOpinions <- mapM (newSongOpinionFromRequest (authUser ^. #identifier)) (request ^. #songOpinions)
    let validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newOpinions

    ifAllValid validationResults $ do
      upsertedOpinions <- upsertSongOpinions env newOpinions
      pure
        . Right
        $ UpsertSongOpinionsCommandResponse
          { songOpinions = upsertedOpinions,
            validationResults = validationResults
          }

insertSongArtworksAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertSongArtworksRequest ->
  Free f (Either SongError InsertSongArtworksCommandResponse)
insertSongArtworksAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let entityValidation x = (x ^. #songIdentifier, validateSongArtwork x)

    newArtworks <- mapM (newSongArtworkFromRequest (authUser ^. #identifier)) (request ^. #songArtworks)

    let validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newArtworks

    ifAllValid validationResults $ do
      insertedArtworks <- insertSongArtworks env newArtworks
      pure
        . Right
        $ InsertSongArtworksCommandResponse
          { songArtworks = insertedArtworks,
            validationResults = validationResults
          }

deleteSongsByIdentifierAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either SongError ())
deleteSongsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteSongs env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

deleteSongCommentsByIdentifierAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either SongError ())
deleteSongCommentsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteSongComments env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

deleteSongOpinionsByIdentifierAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either SongError ())
deleteSongOpinionsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteSongOpinions env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

deleteSongArtworksByIdentifierAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either SongError ())
deleteSongArtworksByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteSongArtworks env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

updateSongArtworksOrderAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  SongArtworkOrderUpdateRequest ->
  Free f (Either SongError ())
updateSongArtworksOrderAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let entityValidation x = (x ^. #identifier, validateSongArtworkOrderUpdate x)
        songArtworkOrderUpdates = request ^. #songArtworkOrders
        validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) songArtworkOrderUpdates

    ifAllValid validationResults $ do
      operationResults <- updateSongArtworkOrder env songArtworkOrderUpdates
      pure . first SomeError $ operationResults

updateSongAction ::
  (SongQuery :<: f, SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  SongDeltaRequest ->
  Free f (Either SongError ())
updateSongAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let entityValidation x = (x ^. #identifier, validateSongDelta x)
        deltas = request ^. #songDeltas
        validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) deltas

    ifAllValid validationResults $ do
      let songIds = map (^. #identifier) deltas
          deltaMap = fromList $ map (\x -> (x ^. #identifier, x)) deltas

      songRecords <- fst <$> fetchSongsByUUID env DescCreatedAt songIds

      let songRecordAndDeltaPairMap = Map.mapWithKey (\k v -> (v, deltaMap Map.!? k)) songRecords

      operationResults <- updateSongs env songRecordAndDeltaPairMap
      pure . first SomeError $ operationResults

insertArtistsOfSongAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertArtistsOfSongsRequest ->
  Free f (Either SongError InsertArtistsOfSongCommandResponse)
insertArtistsOfSongAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let entityValidation x = (x ^. #artistIdentifier, validateArtistOfSong x)
    newArtistsOfSong <- mapM (newArtistOfSongFromRequest (authUser ^. #identifier)) (request ^. #songArtists)
    let validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newArtistsOfSong

    ifAllValid validationResults $ do
      newArtistsOfSongMap <- insertArtistsOfSongs env newArtistsOfSong
      pure
        . Right
        $ InsertArtistsOfSongCommandResponse
          { songArtists = newArtistsOfSongMap,
            validationResults = validationResults
          }

deleteArtistsOfSongAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertArtistsOfSongsRequest ->
  Free f (Either SongError ())
deleteArtistsOfSongAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let isN = nonEmpty (request ^. #songArtists)
    case isN of
      Nothing -> pure . Left . SomeError . pack $ "No valid data provided!"
      Just songArtists -> do
        let r = head songArtists
        operationResults <- deleteArtistOfSong env (r ^. #songIdentifier, r ^. #artistIdentifier)
        pure
          . first (SomeError . pack . Relude.show)
          $ operationResults

updateSongContentsAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  SongContentDeltaRequest ->
  Free f (Either SongError ())
updateSongContentsAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let entityValidation x = (x ^. #identifier, validateSongContentDelta x)
    let deltas = request ^. #songContentDeltas
    let validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) deltas

    ifAllValid validationResults $ do
      operationResults <- updateSongContents env deltas
      pure . first SomeError $ operationResults

deleteSongContentsByIdentifierAction ::
  (SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either SongError ())
deleteSongContentsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteSongContents env [identifier]
    pure
      . first (SomeError . pack . Relude.show)
      $ operationResults

insertSongContentsAction ::
  (SongQuery :<: f, SongCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertSongContentsRequest ->
  Free f (Either SongError InsertSongContentsCommandResponse)
insertSongContentsAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let entityValidation x = ((pack . Relude.show $ x ^. #songIdentifier) <> "-" <> x ^. #versionName, validateSongContent x)

    newSongContents <- mapM (newSongContentFromRequest (authUser ^. #identifier)) (request ^. #songContents)

    let validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newSongContents

    ifAllValid validationResults $ do
      newSongContentsMap <- insertSongContents env newSongContents
      pure
        . Right
        $ InsertSongContentsCommandResponse
          { songContents = newSongContentsMap,
            validationResults = validationResults
          }
