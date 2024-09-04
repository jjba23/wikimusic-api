{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.Interaction.Genre
  ( fetchGenresAction,
    insertGenresAction,
    insertGenreCommentsAction,
    insertGenreArtworksAction,
    upsertGenreOpinionsAction,
    deleteGenresByIdentifierAction,
    deleteGenreCommentsByIdentifierAction,
    deleteGenreOpinionsByIdentifierAction,
    deleteGenreArtworksByIdentifierAction,
    updateGenreArtworksOrderAction,
    updateGenreAction,
    fetchGenreAction,
    searchGenresAction,
  )
where

import Data.Map qualified as Map
import Data.Text (pack, take, unpack)
import Relude
import WikiMusic.Free.GenreCommand
import WikiMusic.Free.GenreQuery
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Model.Genre
import WikiMusic.Model.Other
import WikiMusic.PostgreSQL.GenreCommand ()
import WikiMusic.PostgreSQL.GenreQuery ()
import WikiMusic.Protolude

fetchGenresAction ::
  (GenreQuery :<: f, GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Text ->
  Free f (Either GenreError GetGenresQueryResponse)
fetchGenresAction env authUser limit offset maybeSortOrder maybeInclude =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (genreMap, sortOrderList) <- fetchGenres env sortOrder limit offset

    enrichedGenres <-
      enrichedGenreResponse
        env
        genreMap
        (maybe noEnrichment parseInclude maybeInclude)

    pure . Right $ GetGenresQueryResponse {genres = enrichedGenres, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

searchGenresAction ::
  (GenreQuery :<: f, GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  Free f (Either GenreError GetGenresQueryResponse)
searchGenresAction env authUser limit offset maybeSortOrder maybeInclude searchInput =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (genreMap, sortOrderList) <- searchGenres env (SearchInput searchInput) sortOrder limit offset

    enrichedGenres <-
      enrichedGenreResponse
        env
        genreMap
        (maybe noEnrichment parseInclude maybeInclude)

    pure . Right $ GetGenresQueryResponse {genres = enrichedGenres, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

fetchGenreAction ::
  (GenreQuery :<: f, GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Maybe Text ->
  Maybe Text ->
  Free f (Either GenreError GetGenresQueryResponse)
fetchGenreAction env authUser identifier maybeSortOrder maybeInclude =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    (genreMap, sortOrderList) <- fetchGenresByUUID env sortOrder [identifier]

    enrichedGenres <-
      enrichedGenreResponse
        env
        genreMap
        (maybe noEnrichment parseInclude maybeInclude)

    _ <- incrementViewsByOne env (Map.keys genreMap)

    pure . Right $ GetGenresQueryResponse {genres = enrichedGenres, sortOrder = sortOrderList}
  where
    sortOrder = fromMaybe DescCreatedAt (readMaybe . unpack =<< maybeSortOrder)

insertGenresAction ::
  (GenreCommand :<: f, GenreQuery :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertGenresRequest ->
  Free f (Either GenreError InsertGenresCommandResponse)
insertGenresAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    newGenres <- mapM (newGenreFromRequest (authUser ^. #identifier)) (request ^. #genres)

    let entityValidation x = (x ^. #displayName, validateGenre x)
        validationResults = fromList $ map entityValidation newGenres
        newGenreIdentifiers = map (^. #identifier) newGenres

    ifAllValid validationResults $ do
      _ <- insertGenres env newGenres
      (genreMap, sortOrder) <- fetchGenresByUUID env DescCreatedAt newGenreIdentifiers
      enrichedInsertedGenres <- enrichedGenreResponse env genreMap fullEnrichment
      pure
        . Right
        $ InsertGenresQueryResponse
          { genres = enrichedInsertedGenres,
            sortOrder = sortOrder,
            validationResults = validationResults
          }

insertGenreCommentsAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertGenreCommentsRequest ->
  Free f (Either GenreError InsertGenreCommentsCommandResponse)
insertGenreCommentsAction env authUser request =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    newComments <- mapM (newGenreCommentFromRequest (authUser ^. #identifier)) (request ^. #genreComments)

    let entityValidation x = (Data.Text.take 20 (x ^. #comment % #contents), validateGenreComment x)
        validationResults = fromList $ map entityValidation newComments

    ifAllValid validationResults $ do
      insertedComments <- insertGenreComments env newComments
      pure
        . Right
        $ InsertGenreCommentsCommandResponse
          { genreComments = insertedComments,
            validationResults = validationResults
          }

upsertGenreOpinionsAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UpsertGenreOpinionsRequest ->
  Free f (Either GenreError UpsertGenreOpinionsCommandResponse)
upsertGenreOpinionsAction env authUser request =
  doWithRoles' authUser isAtLeastDemo AccessUnauthorizedError $ do
    newOpinions <- mapM (newGenreOpinionFromRequest (authUser ^. #identifier)) (request ^. #genreOpinions)

    let entityValidation x = (x ^. #genreIdentifier, validateGenreOpinion x)
        validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newOpinions

    ifAllValid validationResults $ do
      upsertedOpinions <- upsertGenreOpinions env newOpinions
      pure
        . Right
        $ UpsertGenreOpinionsCommandResponse
          { genreOpinions = upsertedOpinions,
            validationResults = validationResults
          }

insertGenreArtworksAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InsertGenreArtworksRequest ->
  Free f (Either GenreError InsertGenreArtworksCommandResponse)
insertGenreArtworksAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    newArtworks <- mapM (newGenreArtworkFromRequest (authUser ^. #identifier)) (request ^. #genreArtworks)

    let entityValidation x = (x ^. #genreIdentifier, validateGenreArtwork x)
        validationResults = fromList $ map (first (pack . Relude.show) . entityValidation) newArtworks

    ifAllValid validationResults $ do
      insertedArtworks <- insertGenreArtworks env newArtworks
      pure
        . Right
        $ InsertGenreArtworksCommandResponse
          { genreArtworks = insertedArtworks,
            validationResults = validationResults
          }

deleteGenresByIdentifierAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either GenreError ())
deleteGenresByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteGenres env [identifier]
    pure . void $ first (SomeError . pack . Relude.show) operationResults

deleteGenreCommentsByIdentifierAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either GenreError ())
deleteGenreCommentsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteGenreComments env [identifier]
    pure . void $ first (SomeError . pack . Relude.show) operationResults

deleteGenreOpinionsByIdentifierAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either GenreError ())
deleteGenreOpinionsByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteGenreOpinions env [identifier]
    pure . void $ first (SomeError . pack . Relude.show) operationResults

deleteGenreArtworksByIdentifierAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  UUID ->
  Free f (Either GenreError ())
deleteGenreArtworksByIdentifierAction env authUser identifier =
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    operationResults <- deleteGenreArtworks env [identifier]
    pure . void $ first (SomeError . pack . Relude.show) operationResults

updateGenreArtworksOrderAction ::
  (GenreCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  GenreArtworkOrderUpdateRequest ->
  Free f (Either GenreError ())
updateGenreArtworksOrderAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let genreArtworkOrderUpdates = request ^. #genreArtworkOrders
        entityValidation x = (x ^. #identifier, validateGenreArtworkOrderUpdate x)
        validationResults = fromList . map (first (pack . Relude.show) . entityValidation) $ genreArtworkOrderUpdates

    ifAllValid validationResults $ do
      operationResults <- updateGenreArtworkOrder env genreArtworkOrderUpdates
      pure . void $ first SomeError operationResults

updateGenreAction ::
  (GenreCommand :<: f, GenreQuery :<: f) =>
  Env ->
  WikiMusicUser ->
  GenreDeltaRequest ->
  Free f (Either GenreError ())
updateGenreAction env authUser request =
  doWithRoles' authUser isAtLeastLowRank AccessUnauthorizedError $ do
    let δ = request ^. #genreDeltas
        entityValidation x = (x ^. #identifier, validateGenreDelta x)
        validationResults = fromList . map (first (pack . Relude.show) . entityValidation) $ δ

    ifAllValid validationResults $ do
      let genreIds = map (^. #identifier) δ
          deltaMap = fromList $ map (\x -> (x ^. #identifier, x)) δ

      genreRecords <- fst <$> fetchGenresByUUID env DescCreatedAt genreIds

      let genreRecordAndDeltaPairMap = Map.mapWithKey (\k v -> (v, deltaMap Map.!? k)) genreRecords

      operationResults <- updateGenres env genreRecordAndDeltaPairMap
      pure . void $ first SomeError operationResults
