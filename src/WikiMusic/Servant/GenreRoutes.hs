{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Servant.GenreRoutes
  ( fetchGenresRoute,
    fetchGenreRoute,
    insertGenresRoute,
    insertGenreCommentsRoute,
    insertGenreArtworksRoute,
    upsertGenreOpinionsRoute,
    deleteGenresByIdentifierRoute,
    deleteGenreCommentsByIdentifierRoute,
    deleteGenreOpinionsByIdentifierRoute,
    deleteGenreArtworksByIdentifierRoute,
    updateGenreArtworksOrderRoute,
    updateGenreRoute,
    searchGenresRoute,
  )
where

import Servant
import WikiMusic.Free.GenreCommand
import WikiMusic.Free.GenreQuery
import WikiMusic.Interaction.Genre
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Model.Other
import WikiMusic.SQLite.GenreCommand ()
import WikiMusic.SQLite.GenreQuery ()
import WikiMusic.Protolude
import WikiMusic.Servant.Utilities

fetchGenresRoute :: Env -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler GetGenresQueryResponse
fetchGenresRoute env authToken limit offset sort' include =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(GenreQuery :+: GenreCommand) $ fetchGenresAction env authUser (maybe (Limit 10) Limit limit) (maybe (Offset 0) Offset offset) sort' include) >>= maybe200
    )

searchGenresRoute :: Env -> Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler GetGenresQueryResponse
searchGenresRoute env authToken searchInput limit offset sort' include =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(GenreQuery :+: GenreCommand) $ searchGenresAction env authUser (maybe (Limit 10) Limit limit) (maybe (Offset 0) Offset offset) sort' include searchInput) >>= maybe200
    )

fetchGenreRoute :: Env -> Maybe Text -> UUID -> Handler GetGenresQueryResponse
fetchGenreRoute env authToken identifier =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(GenreQuery :+: GenreCommand) $ fetchGenreAction env authUser identifier (Just "created-at-desc") (Just "artworks,opinions,comments")) >>= maybe200
    )

insertGenresRoute :: Env -> Maybe Text -> InsertGenresRequest -> Handler InsertGenresCommandResponse
insertGenresRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(GenreCommand :+: GenreQuery) $ insertGenresAction env authUser req) >>= maybe204
    )

insertGenreCommentsRoute :: Env -> Maybe Text -> InsertGenreCommentsRequest -> Handler InsertGenreCommentsCommandResponse
insertGenreCommentsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ insertGenreCommentsAction env authUser req) >>= maybe204
    )

upsertGenreOpinionsRoute :: Env -> Maybe Text -> UpsertGenreOpinionsRequest -> Handler UpsertGenreOpinionsCommandResponse
upsertGenreOpinionsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ upsertGenreOpinionsAction env authUser req) >>= maybe204
    )

insertGenreArtworksRoute :: Env -> Maybe Text -> InsertGenreArtworksRequest -> Handler InsertGenreArtworksCommandResponse
insertGenreArtworksRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ insertGenreArtworksAction env authUser req) >>= maybe204
    )

deleteGenresByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteGenresByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ deleteGenresByIdentifierAction env authUser uid) >>= maybe204
    )

deleteGenreCommentsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteGenreCommentsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ deleteGenreCommentsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteGenreOpinionsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteGenreOpinionsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ deleteGenreOpinionsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteGenreArtworksByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteGenreArtworksByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ deleteGenreArtworksByIdentifierAction env authUser uid) >>= maybe204
    )

updateGenreArtworksOrderRoute :: Env -> Maybe Text -> GenreArtworkOrderUpdateRequest -> Handler ()
updateGenreArtworksOrderRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @GenreCommand $ updateGenreArtworksOrderAction env authUser req) >>= maybe204
    )

updateGenreRoute :: Env -> Maybe Text -> GenreDeltaRequest -> Handler ()
updateGenreRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(GenreCommand :+: GenreQuery) $ updateGenreAction env authUser req) >>= maybe204
    )
