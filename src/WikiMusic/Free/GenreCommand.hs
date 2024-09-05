module WikiMusic.Free.GenreCommand
  ( GenreCommand (..),
    insertGenres,
    insertGenreComments,
    insertGenreArtworks,
    upsertGenreOpinions,
    insertGenreExternalSources,
    deleteGenres,
    deleteGenreComments,
    deleteGenreArtworks,
    deleteGenreOpinions,
    deleteCommentsOfGenres,
    deleteGenreExternalSources,
    deleteArtworksOfGenres,
    deleteOpinionsOfGenres,
    updateGenreArtworkOrder,
    updateGenres,
    updateGenreExternalSources,
    newGenreFromRequest,
    newGenreCommentFromRequest,
    newGenreOpinionFromRequest,
    newGenreArtworkFromRequest,
    GenreCommandError (..),
    incrementViewsByOne,
  )
where

import Free.AlaCarte
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Model.Genre
import WikiMusic.Protolude

data GenreCommandError = PersistenceError Text | LogicError Text
  deriving (Eq, Show)

type GenreCommand :: Type -> Type
data GenreCommand a
  = InsertGenres Env [Genre] (Map UUID Genre -> a)
  | InsertGenreComments Env [GenreComment] (Map UUID GenreComment -> a)
  | InsertGenreArtworks Env [GenreArtwork] (Map UUID GenreArtwork -> a)
  | UpsertGenreOpinions Env [GenreOpinion] (Map UUID GenreOpinion -> a)
  | InsertGenreExternalSources Env [GenreExternalSources] (Map UUID GenreExternalSources -> a)
  | DeleteGenres Env [UUID] (Either GenreCommandError () -> a)
  | DeleteGenreComments Env [UUID] (Either GenreCommandError () -> a)
  | DeleteGenreArtworks Env [UUID] (Either GenreCommandError () -> a)
  | DeleteGenreOpinions Env [UUID] (Either GenreCommandError () -> a)
  | DeleteCommentsOfGenres Env [UUID] (Either GenreCommandError () -> a)
  | DeleteGenreExternalSources Env [UUID] (Either GenreCommandError () -> a)
  | DeleteArtworksOfGenres Env [UUID] (Either GenreCommandError () -> a)
  | DeleteOpinionsOfGenres Env [UUID] (Either GenreCommandError () -> a)
  | UpdateGenreArtworkOrder Env [GenreArtworkOrderUpdate] (Either Text () -> a)
  | UpdateGenres Env (Map UUID (Genre, Maybe GenreDelta)) (Either Text () -> a)
  | UpdateGenreExternalSources Env (Map UUID (Genre, Maybe GenreDelta)) (Either Text () -> a)
  | NewGenreFromRequest UUID InsertGenresRequestItem (Genre -> a)
  | -- TODO: separate model generators to their own monad
    NewGenreCommentFromRequest UUID InsertGenreCommentsRequestItem (GenreComment -> a)
  | NewGenreOpinionFromRequest UUID UpsertGenreOpinionsRequestItem (GenreOpinion -> a)
  | NewGenreArtworkFromRequest UUID InsertGenreArtworksRequestItem (GenreArtwork -> a)
  | IncrementViewsByOne Env [UUID] (Either GenreCommandError () -> a)
  deriving (Functor)

insertGenres :: (GenreCommand :<: f) => Env -> [Genre] -> Free f (Map UUID Genre)
insertGenres env genres = injectFree (InsertGenres env genres Pure)

insertGenreComments :: (GenreCommand :<: f) => Env -> [GenreComment] -> Free f (Map UUID GenreComment)
insertGenreComments env genreComments = injectFree (InsertGenreComments env genreComments Pure)

insertGenreArtworks :: (GenreCommand :<: f) => Env -> [GenreArtwork] -> Free f (Map UUID GenreArtwork)
insertGenreArtworks env genreArtworks = injectFree (InsertGenreArtworks env genreArtworks Pure)

upsertGenreOpinions :: (GenreCommand :<: f) => Env -> [GenreOpinion] -> Free f (Map UUID GenreOpinion)
upsertGenreOpinions env genreOpinions = injectFree (UpsertGenreOpinions env genreOpinions Pure)

insertGenreExternalSources :: (GenreCommand :<: f) => Env -> [GenreExternalSources] -> Free f (Map UUID GenreExternalSources)
insertGenreExternalSources env genreExternalSources = injectFree (InsertGenreExternalSources env genreExternalSources Pure)

deleteGenres :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteGenres env uuids = injectFree (DeleteGenres env uuids Pure)

deleteGenreComments :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteGenreComments env uuids = injectFree (DeleteGenreComments env uuids Pure)

deleteGenreArtworks :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteGenreArtworks env uuids = injectFree (DeleteGenreArtworks env uuids Pure)

deleteGenreOpinions :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteGenreOpinions env uuids = injectFree (DeleteGenreOpinions env uuids Pure)

deleteCommentsOfGenres :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteCommentsOfGenres env uuids = injectFree (DeleteCommentsOfGenres env uuids Pure)

deleteGenreExternalSources :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteGenreExternalSources env uuids = injectFree (DeleteGenreExternalSources env uuids Pure)

deleteArtworksOfGenres :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteArtworksOfGenres env uuids = injectFree (DeleteArtworksOfGenres env uuids Pure)

deleteOpinionsOfGenres :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
deleteOpinionsOfGenres env uuids = injectFree (DeleteOpinionsOfGenres env uuids Pure)

updateGenreArtworkOrder :: (GenreCommand :<: f) => Env -> [GenreArtworkOrderUpdate] -> Free f (Either Text ())
updateGenreArtworkOrder env uuids = injectFree (UpdateGenreArtworkOrder env uuids Pure)

updateGenres :: (GenreCommand :<: f) => Env -> Map UUID (Genre, Maybe GenreDelta) -> Free f (Either Text ())
updateGenres env deltas = injectFree (UpdateGenres env deltas Pure)

updateGenreExternalSources :: (GenreCommand :<: f) => Env -> Map UUID (Genre, Maybe GenreDelta) -> Free f (Either Text ())
updateGenreExternalSources env deltas = injectFree (UpdateGenreExternalSources env deltas Pure)

newGenreFromRequest :: (GenreCommand :<: f) => UUID -> InsertGenresRequestItem -> Free f Genre
newGenreFromRequest uuid req = injectFree (NewGenreFromRequest uuid req Pure)

newGenreCommentFromRequest :: (GenreCommand :<: f) => UUID -> InsertGenreCommentsRequestItem -> Free f GenreComment
newGenreCommentFromRequest uuid req = injectFree (NewGenreCommentFromRequest uuid req Pure)

newGenreOpinionFromRequest :: (GenreCommand :<: f) => UUID -> UpsertGenreOpinionsRequestItem -> Free f GenreOpinion
newGenreOpinionFromRequest uuid req = injectFree (NewGenreOpinionFromRequest uuid req Pure)

newGenreArtworkFromRequest :: (GenreCommand :<: f) => UUID -> InsertGenreArtworksRequestItem -> Free f GenreArtwork
newGenreArtworkFromRequest uuid req = injectFree (NewGenreArtworkFromRequest uuid req Pure)

incrementViewsByOne :: (GenreCommand :<: f) => Env -> [UUID] -> Free f (Either GenreCommandError ())
incrementViewsByOne env uuids = injectFree (IncrementViewsByOne env uuids Pure)
