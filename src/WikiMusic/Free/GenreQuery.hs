{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.GenreQuery
  ( GenreQuery (..),
    fetchGenres,
    fetchGenresByUUID,
    enrichedGenreResponse,
    fetchGenreComments,
    fetchGenreOpinions,
    fetchGenreArtworks,
    GenreQueryError (..),
    searchGenres,
  )
where

import Free.AlaCarte
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Model.Genre
import WikiMusic.Model.Other
import WikiMusic.Protolude

data GenreQueryError = PersistenceError Text | LogicError Text
  deriving (Eq, Show)

type GenreQuery :: Type -> Type
data GenreQuery a
  = FetchGenres Env GenreSortOrder Limit Offset ((Map UUID Genre, [UUID]) -> a)
  | FetchGenresByUUID Env GenreSortOrder [UUID] ((Map UUID Genre, [UUID]) -> a)
  | EnrichedGenreResponse Env (Map UUID Genre) EnrichGenreParams (Map UUID Genre -> a)
  | FetchGenreComments Env [UUID] (Map UUID GenreComment -> a)
  | FetchGenreOpinions Env [UUID] (Map UUID GenreOpinion -> a)
  | FetchGenreArtworks Env [UUID] (Map UUID GenreArtwork -> a)
  | SearchGenres Env SearchInput GenreSortOrder Limit Offset ((Map UUID Genre, [UUID]) -> a)
  deriving (Functor)

fetchGenres :: (GenreQuery :<: f) => Env -> GenreSortOrder -> Limit -> Offset -> Free f (Map UUID Genre, [UUID])
fetchGenres env sortOrder limit offset = injectFree (FetchGenres env sortOrder limit offset Pure)

fetchGenresByUUID :: (GenreQuery :<: f) => Env -> GenreSortOrder -> [UUID] -> Free f (Map UUID Genre, [UUID])
fetchGenresByUUID env sortOrder uuids = injectFree (FetchGenresByUUID env sortOrder uuids Pure)

enrichedGenreResponse :: (GenreQuery :<: f) => Env -> Map UUID Genre -> EnrichGenreParams -> Free f (Map UUID Genre)
enrichedGenreResponse env genres enrichParams = injectFree (EnrichedGenreResponse env genres enrichParams Pure)

fetchGenreComments :: (GenreQuery :<: f) => Env -> [UUID] -> Free f (Map UUID GenreComment)
fetchGenreComments env uuids = injectFree (FetchGenreComments env uuids Pure)

fetchGenreOpinions :: (GenreQuery :<: f) => Env -> [UUID] -> Free f (Map UUID GenreOpinion)
fetchGenreOpinions env uuids = injectFree (FetchGenreOpinions env uuids Pure)

fetchGenreArtworks :: (GenreQuery :<: f) => Env -> [UUID] -> Free f (Map UUID GenreArtwork)
fetchGenreArtworks env uuids = injectFree (FetchGenreArtworks env uuids Pure)

searchGenres :: (GenreQuery :<: f) => Env -> SearchInput -> GenreSortOrder -> Limit -> Offset -> Free f (Map UUID Genre, [UUID])
searchGenres env searchInput sortOrder limit offset = injectFree (SearchGenres env searchInput sortOrder limit offset Pure)
