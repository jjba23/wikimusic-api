{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Servant.ArtistRoutes
  ( fetchArtistsRoute,
    fetchArtistRoute,
    insertArtistsRoute,
    insertArtistCommentsRoute,
    insertArtistArtworksRoute,
    upsertArtistOpinionsRoute,
    deleteArtistsByIdentifierRoute,
    deleteArtistCommentsByIdentifierRoute,
    deleteArtistOpinionsByIdentifierRoute,
    deleteArtistArtworksByIdentifierRoute,
    updateArtistArtworksOrderRoute,
    updateArtistRoute,
    searchArtistsRoute,
  )
where

import Servant
import WikiMusic.Free.ArtistCommand
import WikiMusic.Free.ArtistQuery
import WikiMusic.Interaction.Artist
import WikiMusic.Interaction.Model.Artist
import WikiMusic.Model.Other
import WikiMusic.Sqlite.ArtistCommand ()
import WikiMusic.Sqlite.ArtistQuery ()
import WikiMusic.Protolude
import WikiMusic.Servant.Utilities

fetchArtistsRoute :: Env -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler GetArtistsQueryResponse
fetchArtistsRoute env authToken limit offset sort' include =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(ArtistQuery :+: ArtistCommand) $ fetchArtistsAction env authUser (maybe (Limit 10) Limit limit) (maybe (Offset 0) Offset offset) sort' include) >>= maybe200
    )

searchArtistsRoute :: Env -> Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler GetArtistsQueryResponse
searchArtistsRoute env authToken searchInput limit offset sort' include =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(ArtistQuery :+: ArtistCommand) $ searchArtistsAction env authUser (maybe (Limit 10) Limit limit) (maybe (Offset 0) Offset offset) sort' include searchInput) >>= maybe200
    )

fetchArtistRoute :: Env -> Maybe Text -> UUID -> Handler GetArtistsQueryResponse
fetchArtistRoute env authToken identifier =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(ArtistQuery :+: ArtistCommand) $ fetchArtistAction env authUser identifier (Just "created-at-desc") (Just "artworks,opinions,comments")) >>= maybe200
    )

insertArtistsRoute :: Env -> Maybe Text -> InsertArtistsRequest -> Handler InsertArtistsCommandResponse
insertArtistsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(ArtistCommand :+: ArtistQuery) $ insertArtistsAction env authUser req) >>= maybe204
    )

insertArtistCommentsRoute :: Env -> Maybe Text -> InsertArtistCommentsRequest -> Handler InsertArtistCommentsCommandResponse
insertArtistCommentsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ insertArtistCommentsAction env authUser req) >>= maybe204
    )

upsertArtistOpinionsRoute :: Env -> Maybe Text -> UpsertArtistOpinionsRequest -> Handler UpsertArtistOpinionsCommandResponse
upsertArtistOpinionsRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ upsertArtistOpinionsAction env authUser req) >>= maybe204
    )

insertArtistArtworksRoute :: Env -> Maybe Text -> InsertArtistArtworksRequest -> Handler InsertArtistArtworksCommandResponse
insertArtistArtworksRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ insertArtistArtworksAction env authUser req) >>= maybe204
    )

deleteArtistsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteArtistsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ deleteArtistsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteArtistCommentsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteArtistCommentsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ deleteArtistCommentsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteArtistOpinionsByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteArtistOpinionsByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ deleteArtistOpinionsByIdentifierAction env authUser uid) >>= maybe204
    )

deleteArtistArtworksByIdentifierRoute :: Env -> Maybe Text -> UUID -> Handler ()
deleteArtistArtworksByIdentifierRoute env authToken uid =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ deleteArtistArtworksByIdentifierAction env authUser uid) >>= maybe204
    )

updateArtistArtworksOrderRoute :: Env -> Maybe Text -> ArtistArtworkOrderUpdateRequest -> Handler ()
updateArtistArtworksOrderRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @ArtistCommand $ updateArtistArtworksOrderAction env authUser req) >>= maybe204
    )

updateArtistRoute :: Env -> Maybe Text -> ArtistDeltaRequest -> Handler ()
updateArtistRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(ArtistCommand :+: ArtistQuery) $ updateArtistAction env authUser req) >>= maybe204
    )
