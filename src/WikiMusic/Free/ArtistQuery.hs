{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.ArtistQuery
  ( ArtistQuery (..),
    fetchArtists,
    fetchArtistsByUUID,
    enrichedArtistResponse,
    fetchArtistComments,
    fetchArtistOpinions,
    fetchArtistArtworks,
    ArtistQueryError (..),
    searchArtists,
  )
where

import WikiMusic.Interaction.Model.Artist
import WikiMusic.Model.Artist
import WikiMusic.Model.Other
import WikiMusic.Protolude

data ArtistQueryError = PersistenceError Text | LogicError Text
  deriving (Eq, Show)

type ArtistQuery :: Type -> Type
data ArtistQuery a
  = FetchArtists Env ArtistSortOrder Limit Offset ((Map UUID Artist, [UUID]) -> a)
  | FetchArtistsByUUID Env ArtistSortOrder [UUID] ((Map UUID Artist, [UUID]) -> a)
  | EnrichedArtistResponse Env (Map UUID Artist) EnrichArtistParams (Map UUID Artist -> a)
  | FetchArtistComments Env [UUID] (Map UUID ArtistComment -> a)
  | FetchArtistOpinions Env [UUID] (Map UUID ArtistOpinion -> a)
  | FetchArtistArtworks Env [UUID] (Map UUID ArtistArtwork -> a)
  | SearchArtists Env SearchInput ArtistSortOrder Limit Offset ((Map UUID Artist, [UUID]) -> a)
  deriving (Functor)

fetchArtists :: (ArtistQuery :<: f) => Env -> ArtistSortOrder -> Limit -> Offset -> Free f (Map UUID Artist, [UUID])
fetchArtists env sortOrder limit offset = injectFree (FetchArtists env sortOrder limit offset Pure)

fetchArtistsByUUID :: (ArtistQuery :<: f) => Env -> ArtistSortOrder -> [UUID] -> Free f (Map UUID Artist, [UUID])
fetchArtistsByUUID env sortOrder uuids = injectFree (FetchArtistsByUUID env sortOrder uuids Pure)

enrichedArtistResponse :: (ArtistQuery :<: f) => Env -> Map UUID Artist -> EnrichArtistParams -> Free f (Map UUID Artist)
enrichedArtistResponse env artists enrichParams = injectFree (EnrichedArtistResponse env artists enrichParams Pure)

fetchArtistComments :: (ArtistQuery :<: f) => Env -> [UUID] -> Free f (Map UUID ArtistComment)
fetchArtistComments env uuids = injectFree (FetchArtistComments env uuids Pure)

fetchArtistOpinions :: (ArtistQuery :<: f) => Env -> [UUID] -> Free f (Map UUID ArtistOpinion)
fetchArtistOpinions env uuids = injectFree (FetchArtistOpinions env uuids Pure)

fetchArtistArtworks :: (ArtistQuery :<: f) => Env -> [UUID] -> Free f (Map UUID ArtistArtwork)
fetchArtistArtworks env uuids = injectFree (FetchArtistArtworks env uuids Pure)

searchArtists :: (ArtistQuery :<: f) => Env -> SearchInput -> ArtistSortOrder -> Limit -> Offset -> Free f (Map UUID Artist, [UUID])
searchArtists env searchInput sortOrder limit offset = injectFree (SearchArtists env searchInput sortOrder limit offset Pure)
