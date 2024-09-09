{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Servant.SongRoutes
  ( fetchSongsRoute,
    insertSongsRoute,
    insertSongCommentsRoute,
    insertSongArtworksRoute,
    upsertSongOpinionsRoute,
    deleteSongsByIdentifierRoute,
    deleteSongCommentsByIdentifierRoute,
    deleteSongOpinionsByIdentifierRoute,
    deleteSongArtworksByIdentifierRoute,
    updateSongArtworksOrderRoute,
    updateSongRoute,
    insertArtistOfSongRoute,
    fetchSongRoute,
    updateSongContentsRoute,
    insertSongContentsRoute,
    deleteSongContentsByIdentifierRoute,
    searchSongsRoute,
    deleteArtistOfSongRoute,
  )
where

import Servant
import WikiMusic.Console.Logger ()
import WikiMusic.Free.Logger
import WikiMusic.Free.SongCommand
import WikiMusic.Free.SongQuery
import WikiMusic.Interaction.Model.Song
import WikiMusic.Interaction.Song
import WikiMusic.Model.Env
import WikiMusic.Model.Other
import WikiMusic.Protolude
import WikiMusic.Servant.Utilities
import WikiMusic.Sqlite.SongCommand ()
import WikiMusic.Sqlite.SongQuery ()

fetchSongsRoute :: Env -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler GetSongsQueryResponse
fetchSongsRoute env authToken limit offset sort' include =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(SongQuery :+: SongCommand) $ fetchSongsAction env authUser (maybe (Limit 25) Limit limit) (maybe (Offset 0) Offset offset) sort' include) >>= maybe200
    )

searchSongsRoute :: Env -> Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler GetSongsQueryResponse
searchSongsRoute env authToken searchInput limit offset sort' include =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(SongQuery :+: SongCommand) $ searchSongsAction env authUser (maybe (Limit 25) Limit limit) (maybe (Offset 0) Offset offset) sort' include searchInput) >>= maybe200
    )

fetchSongRoute :: Env -> Maybe Text -> UUID -> Handler GetSongsQueryResponse
fetchSongRoute env authToken identifier =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(SongQuery :+: SongCommand) $ fetchSongAction env authUser identifier (Just "created-at-desc") (Just "artworks,opinions,comments,contents")) >>= maybe200
    )

insertSongsRoute :: Env -> Maybe Text -> InsertSongsRequest -> Handler InsertSongsCommandResponse
insertSongsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(SongCommand :+: SongQuery :+: Logger) $ insertSongsAction env authUser req) >>= maybe204
    )

insertSongCommentsRoute :: Env -> Maybe Text -> InsertSongCommentsRequest -> Handler InsertSongCommentsCommandResponse
insertSongCommentsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ insertSongCommentsAction env authUser req) >>= maybe204
    )

upsertSongOpinionsRoute :: Env -> Maybe Text -> UpsertSongOpinionsRequest -> Handler UpsertSongOpinionsCommandResponse
upsertSongOpinionsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ upsertSongOpinionsAction env authUser req) >>= maybe204
    )

insertSongArtworksRoute :: Env -> Maybe Text -> InsertSongArtworksRequest -> Handler InsertSongArtworksCommandResponse
insertSongArtworksRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ insertSongArtworksAction env authUser req) >>= maybe204
    )

deleteSongsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteSongsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ deleteSongsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteSongCommentsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteSongCommentsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ deleteSongCommentsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteSongOpinionsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteSongOpinionsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ deleteSongOpinionsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteSongArtworksByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteSongArtworksByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ deleteSongArtworksByIdentifierAction env authUser uid) >>= maybe204
    )

updateSongArtworksOrderRoute :: Env -> Maybe Text -> SongArtworkOrderUpdateRequest -> Handler ()
updateSongArtworksOrderRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ updateSongArtworksOrderAction env authUser req) >>= maybe204
    )

updateSongRoute :: Env -> Maybe Text -> SongDeltaRequest -> Handler ()
updateSongRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(SongCommand :+: SongQuery) $ updateSongAction env authUser req) >>= maybe204
    )

insertArtistOfSongRoute :: Env -> Maybe Text -> InsertArtistsOfSongsRequest -> Handler InsertArtistsOfSongCommandResponse
insertArtistOfSongRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ insertArtistsOfSongAction env authUser req) >>= maybe204
    )

deleteArtistOfSongRoute :: Env -> Maybe Text -> InsertArtistsOfSongsRequest -> Handler ()
deleteArtistOfSongRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ deleteArtistsOfSongAction env authUser req) >>= maybe204
    )

insertSongContentsRoute :: Env -> Maybe Text -> InsertSongContentsRequest -> Handler InsertSongContentsCommandResponse
insertSongContentsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(SongCommand :+: SongQuery) $ insertSongContentsAction env authUser req) >>= maybe204
    )

deleteSongContentsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteSongContentsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ deleteSongContentsByIdentifierAction env authUser uid) >>= maybe204
    )

updateSongContentsRoute :: Env -> Maybe Text -> SongContentDeltaRequest -> Handler ()
updateSongContentsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @SongCommand $ updateSongContentsAction env authUser req) >>= maybe204
    )
