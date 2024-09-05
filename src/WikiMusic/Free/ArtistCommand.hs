module WikiMusic.Free.ArtistCommand
  ( ArtistCommand (..),
    insertArtists,
    insertArtistComments,
    insertArtistArtworks,
    upsertArtistOpinions,
    insertArtistExternalSources,
    deleteArtists,
    deleteArtistComments,
    deleteArtistArtworks,
    deleteArtistOpinions,
    deleteCommentsOfArtists,
    deleteArtistExternalSources,
    deleteArtworksOfArtists,
    deleteOpinionsOfArtists,
    updateArtistArtworkOrder,
    updateArtists,
    updateArtistExternalSources,
    newArtistFromRequest,
    newArtistCommentFromRequest,
    newArtistOpinionFromRequest,
    newArtistArtworkFromRequest,
    ArtistCommandError (..),
    incrementViewsByOne,
  )
where

import WikiMusic.Interaction.Model.Artist
import WikiMusic.Model.Artist
import WikiMusic.Protolude

data ArtistCommandError = PersistenceError Text | LogicError Text
  deriving (Eq, Show)

type ArtistCommand :: Type -> Type
data ArtistCommand a
  = InsertArtists Env [Artist] (() -> a)
  | InsertArtistComments Env [ArtistComment] (() -> a)
  | InsertArtistArtworks Env [ArtistArtwork] (Map UUID ArtistArtwork -> a)
  | UpsertArtistOpinions Env [ArtistOpinion] (Map UUID ArtistOpinion -> a)
  | InsertArtistExternalSources Env [ArtistExternalSources] (Map UUID ArtistExternalSources -> a)
  | DeleteArtists Env [UUID] (Either ArtistCommandError () -> a)
  | DeleteArtistComments Env [UUID] (Either ArtistCommandError () -> a)
  | DeleteArtistArtworks Env [UUID] (Either ArtistCommandError () -> a)
  | DeleteArtistOpinions Env [UUID] (Either ArtistCommandError () -> a)
  | DeleteCommentsOfArtists Env [UUID] (Either ArtistCommandError () -> a)
  | DeleteArtistExternalSources Env [UUID] (Either ArtistCommandError () -> a)
  | DeleteArtworksOfArtists Env [UUID] (Either ArtistCommandError () -> a)
  | DeleteOpinionsOfArtists Env [UUID] (Either ArtistCommandError () -> a)
  | UpdateArtistArtworkOrder Env [ArtistArtworkOrderUpdate] (Either Text () -> a)
  | UpdateArtists Env (Map UUID (Artist, Maybe ArtistDelta)) (Either Text () -> a)
  | UpdateArtistExternalSources Env (Map UUID (Artist, Maybe ArtistDelta)) (Either Text () -> a)
  | NewArtistFromRequest UUID InsertArtistsRequestItem (Artist -> a)
  | NewArtistCommentFromRequest UUID InsertArtistCommentsRequestItem (ArtistComment -> a)
  | NewArtistOpinionFromRequest UUID UpsertArtistOpinionsRequestItem (ArtistOpinion -> a)
  | NewArtistArtworkFromRequest UUID InsertArtistArtworksRequestItem (ArtistArtwork -> a)
  | IncrementViewsByOne Env [UUID] (Either ArtistCommandError () -> a)
  deriving (Functor)

insertArtists :: (ArtistCommand :<: f) => Env -> [Artist] -> Free f ()
insertArtists env artists = injectFree (InsertArtists env artists Pure)

insertArtistComments :: (ArtistCommand :<: f) => Env -> [ArtistComment] -> Free f ()
insertArtistComments env artistComments = injectFree (InsertArtistComments env artistComments Pure)

insertArtistArtworks :: (ArtistCommand :<: f) => Env -> [ArtistArtwork] -> Free f (Map UUID ArtistArtwork)
insertArtistArtworks env artistArtworks = injectFree (InsertArtistArtworks env artistArtworks Pure)

upsertArtistOpinions :: (ArtistCommand :<: f) => Env -> [ArtistOpinion] -> Free f (Map UUID ArtistOpinion)
upsertArtistOpinions env artistOpinions = injectFree (UpsertArtistOpinions env artistOpinions Pure)

insertArtistExternalSources :: (ArtistCommand :<: f) => Env -> [ArtistExternalSources] -> Free f (Map UUID ArtistExternalSources)
insertArtistExternalSources env artistExternalSources = injectFree (InsertArtistExternalSources env artistExternalSources Pure)

deleteArtists :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteArtists env uuids = injectFree (DeleteArtists env uuids Pure)

deleteArtistComments :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteArtistComments env uuids = injectFree (DeleteArtistComments env uuids Pure)

deleteArtistArtworks :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteArtistArtworks env uuids = injectFree (DeleteArtistArtworks env uuids Pure)

deleteArtistOpinions :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteArtistOpinions env uuids = injectFree (DeleteArtistOpinions env uuids Pure)

deleteCommentsOfArtists :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteCommentsOfArtists env uuids = injectFree (DeleteCommentsOfArtists env uuids Pure)

deleteArtistExternalSources :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteArtistExternalSources env uuids = injectFree (DeleteArtistExternalSources env uuids Pure)

deleteArtworksOfArtists :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteArtworksOfArtists env uuids = injectFree (DeleteArtworksOfArtists env uuids Pure)

deleteOpinionsOfArtists :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
deleteOpinionsOfArtists env uuids = injectFree (DeleteOpinionsOfArtists env uuids Pure)

updateArtistArtworkOrder :: (ArtistCommand :<: f) => Env -> [ArtistArtworkOrderUpdate] -> Free f (Either Text ())
updateArtistArtworkOrder env uuids = injectFree (UpdateArtistArtworkOrder env uuids Pure)

updateArtists :: (ArtistCommand :<: f) => Env -> Map UUID (Artist, Maybe ArtistDelta) -> Free f (Either Text ())
updateArtists env deltas = injectFree (UpdateArtists env deltas Pure)

updateArtistExternalSources :: (ArtistCommand :<: f) => Env -> Map UUID (Artist, Maybe ArtistDelta) -> Free f (Either Text ())
updateArtistExternalSources env deltas = injectFree (UpdateArtistExternalSources env deltas Pure)

newArtistFromRequest :: (ArtistCommand :<: f) => UUID -> InsertArtistsRequestItem -> Free f Artist
newArtistFromRequest uuid req = injectFree (NewArtistFromRequest uuid req Pure)

newArtistCommentFromRequest :: (ArtistCommand :<: f) => UUID -> InsertArtistCommentsRequestItem -> Free f ArtistComment
newArtistCommentFromRequest uuid req = injectFree (NewArtistCommentFromRequest uuid req Pure)

newArtistOpinionFromRequest :: (ArtistCommand :<: f) => UUID -> UpsertArtistOpinionsRequestItem -> Free f ArtistOpinion
newArtistOpinionFromRequest uuid req = injectFree (NewArtistOpinionFromRequest uuid req Pure)

newArtistArtworkFromRequest :: (ArtistCommand :<: f) => UUID -> InsertArtistArtworksRequestItem -> Free f ArtistArtwork
newArtistArtworkFromRequest uuid req = injectFree (NewArtistArtworkFromRequest uuid req Pure)

incrementViewsByOne :: (ArtistCommand :<: f) => Env -> [UUID] -> Free f (Either ArtistCommandError ())
incrementViewsByOne env uuids = injectFree (IncrementViewsByOne env uuids Pure)
