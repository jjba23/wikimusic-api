{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.SongCommand
  ( SongCommand (..),
    insertSongs,
    insertSongComments,
    insertSongArtworks,
    upsertSongOpinions,
    insertSongExternalSources,
    deleteSongs,
    deleteSongComments,
    deleteSongArtworks,
    deleteSongOpinions,
    deleteCommentsOfSongs,
    deleteSongExternalSources,
    deleteArtworksOfSongs,
    deleteOpinionsOfSongs,
    insertArtistsOfSongs,
    deleteArtistsOfSongs,
    deleteArtistOfSong,
    updateSongArtworkOrder,
    updateSongs,
    updateSongExternalSources,
    newSongFromRequest,
    newSongCommentFromRequest,
    newSongOpinionFromRequest,
    newSongArtworkFromRequest,
    newArtistOfSongFromRequest,
    SongCommandError (..),
    insertSongContents,
    updateSongContents,
    deleteSongContents,
    deleteContentsOfSongs,
    newSongContentFromRequest,
    incrementViewsByOne,
  )
where

import Data.UUID
import Free.AlaCarte
import Relude
import WikiMusic.Interaction.Model.Song
import WikiMusic.Model.Env
import WikiMusic.Model.Song

data SongCommandError = PersistenceError Text | LogicError Text
  deriving (Eq, Show)

type SongCommand :: Type -> Type
data SongCommand a
  = InsertSongs Env [Song] (Map UUID Song -> a)
  | InsertSongComments Env [SongComment] (Either SongCommandError () -> a)
  | InsertSongArtworks Env [SongArtwork] (Map UUID SongArtwork -> a)
  | InsertArtistsOfSongs Env [ArtistOfSong] (Map UUID ArtistOfSong -> a)
  | InsertSongExternalSources Env [SongExternalSources] (Map UUID SongExternalSources -> a)
  | InsertSongContents Env [SongContent] (Map UUID SongContent -> a)
  | DeleteSongs Env [UUID] (Either SongCommandError () -> a)
  | DeleteSongComments Env [UUID] (Either SongCommandError () -> a)
  | DeleteSongArtworks Env [UUID] (Either SongCommandError () -> a)
  | DeleteSongOpinions Env [UUID] (Either SongCommandError () -> a)
  | DeleteCommentsOfSongs Env [UUID] (Either SongCommandError () -> a)
  | DeleteSongExternalSources Env [UUID] (Either SongCommandError () -> a)
  | DeleteArtworksOfSongs Env [UUID] (Either SongCommandError () -> a)
  | DeleteOpinionsOfSongs Env [UUID] (Either SongCommandError () -> a)
  | DeleteArtistsOfSongs Env [UUID] (Either SongCommandError () -> a)
  | DeleteArtistOfSong Env (UUID, UUID) (Either SongCommandError () -> a)
  | DeleteSongContents Env [UUID] (Either SongCommandError () -> a)
  | DeleteContentsOfSongs Env [UUID] (Either SongCommandError () -> a)
  | UpsertSongOpinions Env [SongOpinion] (Map UUID SongOpinion -> a)
  | UpdateSongArtworkOrder Env [SongArtworkOrderUpdate] (Either Text () -> a)
  | UpdateSongs Env (Map UUID (Song, Maybe SongDelta)) (Either Text () -> a)
  | UpdateSongExternalSources Env (Map UUID (Song, Maybe SongDelta)) (Either Text () -> a)
  | UpdateSongContents Env [SongContentDelta] (Either Text () -> a)
  | NewSongFromRequest UUID InsertSongsRequestItem (Song -> a)
  | -- TODO: separate model generators to their own monad
    NewSongCommentFromRequest UUID InsertSongCommentsRequestItem (SongComment -> a)
  | NewSongOpinionFromRequest UUID UpsertSongOpinionsRequestItem (SongOpinion -> a)
  | NewSongArtworkFromRequest UUID InsertSongArtworksRequestItem (SongArtwork -> a)
  | NewArtistOfSongFromRequest UUID InsertArtistsOfSongsRequestItem (ArtistOfSong -> a)
  | NewSongContentFromRequest UUID InsertSongContentsRequestItem (SongContent -> a)
  | IncrementViewsByOne Env [UUID] (Either SongCommandError () -> a)
  deriving (Functor)

insertSongs :: (SongCommand :<: f) => Env -> [Song] -> Free f (Map UUID Song)
insertSongs env songs = injectFree (InsertSongs env songs Pure)

insertSongComments :: (SongCommand :<: f) => Env -> [SongComment] -> Free f (Either SongCommandError ())
insertSongComments env songComments = injectFree (InsertSongComments env songComments Pure)

insertSongArtworks :: (SongCommand :<: f) => Env -> [SongArtwork] -> Free f (Map UUID SongArtwork)
insertSongArtworks env songArtworks = injectFree (InsertSongArtworks env songArtworks Pure)

upsertSongOpinions :: (SongCommand :<: f) => Env -> [SongOpinion] -> Free f (Map UUID SongOpinion)
upsertSongOpinions env songOpinions = injectFree (UpsertSongOpinions env songOpinions Pure)

insertSongExternalSources :: (SongCommand :<: f) => Env -> [SongExternalSources] -> Free f (Map UUID SongExternalSources)
insertSongExternalSources env songExternalSources = injectFree (InsertSongExternalSources env songExternalSources Pure)

deleteSongs :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteSongs env uuids = injectFree (DeleteSongs env uuids Pure)

deleteSongComments :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteSongComments env uuids = injectFree (DeleteSongComments env uuids Pure)

deleteSongArtworks :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteSongArtworks env uuids = injectFree (DeleteSongArtworks env uuids Pure)

deleteArtistsOfSongs :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteArtistsOfSongs env uuids = injectFree (DeleteArtistsOfSongs env uuids Pure)

deleteArtistOfSong :: (SongCommand :<: f) => Env -> (UUID, UUID) -> Free f (Either SongCommandError ())
deleteArtistOfSong env uuids = injectFree (DeleteArtistOfSong env uuids Pure)

deleteSongContents :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteSongContents env uuids = injectFree (DeleteSongContents env uuids Pure)

deleteContentsOfSongs :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteContentsOfSongs env uuids = injectFree (DeleteContentsOfSongs env uuids Pure)

deleteSongOpinions :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteSongOpinions env uuids = injectFree (DeleteSongOpinions env uuids Pure)

deleteCommentsOfSongs :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteCommentsOfSongs env uuids = injectFree (DeleteCommentsOfSongs env uuids Pure)

deleteSongExternalSources :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteSongExternalSources env uuids = injectFree (DeleteSongExternalSources env uuids Pure)

deleteArtworksOfSongs :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteArtworksOfSongs env uuids = injectFree (DeleteArtworksOfSongs env uuids Pure)

deleteOpinionsOfSongs :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
deleteOpinionsOfSongs env uuids = injectFree (DeleteOpinionsOfSongs env uuids Pure)

insertSongContents :: (SongCommand :<: f) => Env -> [SongContent] -> Free f (Map UUID SongContent)
insertSongContents env contents = injectFree (InsertSongContents env contents Pure)

updateSongArtworkOrder :: (SongCommand :<: f) => Env -> [SongArtworkOrderUpdate] -> Free f (Either Text ())
updateSongArtworkOrder env uuids = injectFree (UpdateSongArtworkOrder env uuids Pure)

updateSongs :: (SongCommand :<: f) => Env -> Map UUID (Song, Maybe SongDelta) -> Free f (Either Text ())
updateSongs env deltas = injectFree (UpdateSongs env deltas Pure)

updateSongExternalSources :: (SongCommand :<: f) => Env -> Map UUID (Song, Maybe SongDelta) -> Free f (Either Text ())
updateSongExternalSources env deltas = injectFree (UpdateSongExternalSources env deltas Pure)

updateSongContents :: (SongCommand :<: f) => Env -> [SongContentDelta] -> Free f (Either Text ())
updateSongContents env deltas = injectFree (UpdateSongContents env deltas Pure)

newSongFromRequest :: (SongCommand :<: f) => UUID -> InsertSongsRequestItem -> Free f Song
newSongFromRequest uuid req = injectFree (NewSongFromRequest uuid req Pure)

newSongCommentFromRequest :: (SongCommand :<: f) => UUID -> InsertSongCommentsRequestItem -> Free f SongComment
newSongCommentFromRequest uuid req = injectFree (NewSongCommentFromRequest uuid req Pure)

newSongOpinionFromRequest :: (SongCommand :<: f) => UUID -> UpsertSongOpinionsRequestItem -> Free f SongOpinion
newSongOpinionFromRequest uuid req = injectFree (NewSongOpinionFromRequest uuid req Pure)

newSongContentFromRequest :: (SongCommand :<: f) => UUID -> InsertSongContentsRequestItem -> Free f SongContent
newSongContentFromRequest uuid req = injectFree (NewSongContentFromRequest uuid req Pure)

newSongArtworkFromRequest :: (SongCommand :<: f) => UUID -> InsertSongArtworksRequestItem -> Free f SongArtwork
newSongArtworkFromRequest uuid req = injectFree (NewSongArtworkFromRequest uuid req Pure)

insertArtistsOfSongs :: (SongCommand :<: f) => Env -> [ArtistOfSong] -> Free f (Map UUID ArtistOfSong)
insertArtistsOfSongs env art = injectFree (InsertArtistsOfSongs env art Pure)

newArtistOfSongFromRequest :: (SongCommand :<: f) => UUID -> InsertArtistsOfSongsRequestItem -> Free f ArtistOfSong
newArtistOfSongFromRequest uuid req = injectFree (NewArtistOfSongFromRequest uuid req Pure)

incrementViewsByOne :: (SongCommand :<: f) => Env -> [UUID] -> Free f (Either SongCommandError ())
incrementViewsByOne env uuids = injectFree (IncrementViewsByOne env uuids Pure)
