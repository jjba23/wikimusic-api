{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.Interaction.Artist
  ( fetchArtistsAction,
    insertArtistsAction,
    insertArtistCommentsAction,
    insertArtistArtworksAction,
    upsertArtistOpinionsAction,
    deleteArtistsByIdentifierAction,
    deleteArtistCommentsByIdentifierAction,
    deleteArtistOpinionsByIdentifierAction,
    deleteArtistArtworksByIdentifierAction,
    updateArtistArtworksOrderAction,
    updateArtistAction,
    fetchArtistAction,
    searchArtistsAction,
  )
where

import Data.Map qualified as Map
import Data.Text (pack, take, unpack)
import Relude
import WikiMusic.Free.ArtistCommand
import WikiMusic.Free.ArtistQuery
import WikiMusic.Interaction.Model.Artist
import WikiMusic.Model.Artist
import WikiMusic.Model.Other
import WikiMusic.Persistence.ArtistCommand ()
import WikiMusic.Persistence.ArtistQuery ()
import WikiMusic.Protolude

fetchArtistsAction ::
  (ArtistQuery :<: f, ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Text ->
  Free f (Either ArtistError GetArtistsQueryResponse)
fetchArtistsAction env authUser limit offset maybeSortOrder maybeInclude =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (artistMap, sortOrderList) <- fetchArtists env sortOrder limit offset

    enrichedArtists <-
      enrichedArtistResponse
        env
        artistMap
        (maybe noEnrichment parseInclude maybeInclude)

    pure . Right $ GetArtistsQueryResponse {artists = enrichedArtists, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

searchArtistsAction ::
  (ArtistQuery :<: f, ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  Free f (Either ArtistError GetArtistsQueryResponse)
searchArtistsAction env authUser limit offset maybeSortOrder maybeInclude searchInput =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (artistMap, sortOrderList) <- searchArtists env (SearchInput searchInput) sortOrder limit offset

    enrichedArtists <-
      enrichedArtistResponse
        env
        artistMap
        (maybe noEnrichment parseInclude maybeInclude)

    pure . Right $ GetArtistsQueryResponse {artists = enrichedArtists, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

fetchArtistAction ::
  (ArtistQuery :<: f, ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Maybe Text ->
  Maybe Text ->
  Free f (Either ArtistError GetArtistsQueryResponse)
fetchArtistAction env authUser identifier maybeSortOrder maybeInclude =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (artistMap, sortOrderList) <- fetchArtistsByUUID env sortOrder [identifier]

    enrichedArtists <-
      enrichedArtistResponse
        env
        artistMap
        (maybe noEnrichment parseInclude maybeInclude)

    _ <- incrementViewsByOne env (Map.keys artistMap)

    pure . Right $ GetArtistsQueryResponse {artists = enrichedArtists, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

insertArtistsAction ::
  (ArtistCommand :<: f, ArtistQuery :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertArtistsRequest ->
  Free f (Either ArtistError InsertArtistsCommandResponse)
insertArtistsAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    newArtists <- mapM (newArtistFromRequest (authUser ^. #identifier)) (request ^. #artists)

    let entityValidation x = (x ^. #displayName, validateArtist x)
        validationResults = fromList $ map entityValidation newArtists
        newArtistIdentifiers = map (^. #identifier) newArtists

    ifAllValid validationResults $ do
      _ <- insertArtists env newArtists
      (artistMap, sortOrder) <- fetchArtistsByUUID env DescCreatedAt newArtistIdentifiers
      enrichedInsertedArtists <- enrichedArtistResponse env artistMap fullEnrichment
      pure
        . Right
        $ InsertArtistsQueryResponse
          { artists = enrichedInsertedArtists,
            sortOrder = sortOrder,
            validationResults = validationResults
          }

insertArtistCommentsAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertArtistCommentsRequest ->
  Free f (Either ArtistError InsertArtistCommentsCommandResponse)
insertArtistCommentsAction env authUser request =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    newComments <- mapM (newArtistCommentFromRequest (authUser ^. #identifier)) (request ^. #artistComments)

    let entityValidation x = (Data.Text.take 20 (x ^. #comment % #contents), validateArtistComment x)
        validationResults = fromList $ map entityValidation newComments

    ifAllValid validationResults $ do
      _ <- insertArtistComments env newComments
      pure
        . Right
        $ InsertArtistCommentsCommandResponse
          { artistComments = Map.empty,
            validationResults = validationResults
          }

upsertArtistOpinionsAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UpsertArtistOpinionsRequest ->
  Free f (Either ArtistError UpsertArtistOpinionsCommandResponse)
upsertArtistOpinionsAction env authUser request =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    newOpinions <- mapM (newArtistOpinionFromRequest (authUser ^. #identifier)) (request ^. #artistOpinions)

    let entityValidation x = (x ^. #artistIdentifier, validateArtistOpinion x)
        validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newOpinions

    ifAllValid validationResults $ do
      upsertedOpinions <- upsertArtistOpinions env newOpinions
      pure
        . Right
        $ UpsertArtistOpinionsCommandResponse
          { artistOpinions = upsertedOpinions,
            validationResults = validationResults
          }

insertArtistArtworksAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertArtistArtworksRequest ->
  Free f (Either ArtistError InsertArtistArtworksCommandResponse)
insertArtistArtworksAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    newArtworks <- mapM (newArtistArtworkFromRequest (authUser ^. #identifier)) (request ^. #artistArtworks)

    let entityValidation x = (x ^. #artistIdentifier, validateArtistArtwork x)
        validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newArtworks

    ifAllValid validationResults $ do
      insertedArtworks <- insertArtistArtworks env newArtworks
      pure
        . Right
        $ InsertArtistArtworksCommandResponse
          { artistArtworks = insertedArtworks,
            validationResults = validationResults
          }

deleteArtistsByIdentifierAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either ArtistError ())
deleteArtistsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteArtists env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

deleteArtistCommentsByIdentifierAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either ArtistError ())
deleteArtistCommentsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteArtistComments env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

deleteArtistOpinionsByIdentifierAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either ArtistError ())
deleteArtistOpinionsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteArtistOpinions env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

deleteArtistArtworksByIdentifierAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either ArtistError ())
deleteArtistArtworksByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteArtistArtworks env [identifier]
    pure . first (SomeError . pack . Relude.show) $ operationResults

updateArtistArtworksOrderAction ::
  (ArtistCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  ArtistArtworkOrderUpdateRequest ->
  Free f (Either ArtistError ())
updateArtistArtworksOrderAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let artistArtworkOrderUpdates = request ^. #artistArtworkOrders
        entityValidation x = (x ^. #identifier, validateArtistArtworkOrderUpdate x)
        validationResults =
          fromList
            $ map (first (pack . Relude.show) . entityValidation) artistArtworkOrderUpdates

    ifAllValid validationResults $ do
      operationResults <- updateArtistArtworkOrder env artistArtworkOrderUpdates
      pure . first SomeError $ operationResults

updateArtistAction ::
  (ArtistCommand :<: f, ArtistQuery :<: f) =>
  Env ->
  WikiMusicUser ->
  ArtistDeltaRequest ->
  Free f (Either ArtistError ())
updateArtistAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let entityValidation x = (x ^. #identifier, validateArtistDelta x)
        deltas = request ^. #artistDeltas
        validationResults = fromList . map (first (pack . Relude.show) . entityValidation) $ deltas

    ifAllValid validationResults $ do
      let artistIds = map (^. #identifier) deltas
          deltaMap = fromList $ map (\x -> (x ^. #identifier, x)) deltas

      artistRecords <- fst <$> fetchArtistsByUUID env DescCreatedAt artistIds

      let artistRecordAndDeltaPairMap = Map.mapWithKey (\k v -> (v, deltaMap Map.!? k)) artistRecords

      operationResults <- updateArtists env artistRecordAndDeltaPairMap
      pure . first SomeError $ operationResults
