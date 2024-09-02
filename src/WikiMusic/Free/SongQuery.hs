{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.SongQuery
  ( SongQuery (..),
    fetchSongs,
    fetchSongsByUUID,
    enrichedSongResponse,
    fetchSongComments,
    fetchSongOpinions,
    fetchSongArtworks,
    SongQueryError (..),
    searchSongs,
    fetchSongContents,
    fetchSongArtists,
  )
where

import Free.AlaCarte
import WikiMusic.Interaction.Model.Song
import WikiMusic.Model.Other
import WikiMusic.Model.Song
import WikiMusic.Protolude

data SongQueryError = PersistenceError Text | LogicError Text
  deriving (Eq, Show)

type SongQuery :: Type -> Type
data SongQuery a
  = FetchSongs Env SongSortOrder Limit Offset ((Map UUID Song, [UUID]) -> a)
  | FetchSongsByUUID Env SongSortOrder [UUID] ((Map UUID Song, [UUID]) -> a)
  | EnrichedSongResponse Env (Map UUID Song) EnrichSongParams (Map UUID Song -> a)
  | FetchSongComments Env [UUID] (Map UUID SongComment -> a)
  | FetchSongOpinions Env [UUID] (Map UUID SongOpinion -> a)
  | FetchSongArtworks Env [UUID] (Map UUID SongArtwork -> a)
  | FetchSongContents Env [UUID] (Map UUID SongContent -> a)
  | FetchSongArtists Env [UUID] ([(UUID, UUID, Text)] -> a)
  | SearchSongs Env SearchInput SongSortOrder Limit Offset ((Map UUID Song, [UUID]) -> a)
  deriving (Functor)

fetchSongs :: (SongQuery :<: f) => Env -> SongSortOrder -> Limit -> Offset -> Free f (Map UUID Song, [UUID])
fetchSongs env sortOrder fetchLimit offset = injectFree (FetchSongs env sortOrder fetchLimit offset Pure)

fetchSongsByUUID :: (SongQuery :<: f) => Env -> SongSortOrder -> [UUID] -> Free f (Map UUID Song, [UUID])
fetchSongsByUUID env sortOrder uuids = injectFree (FetchSongsByUUID env sortOrder uuids Pure)

enrichedSongResponse :: (SongQuery :<: f) => Env -> Map UUID Song -> EnrichSongParams -> Free f (Map UUID Song)
enrichedSongResponse env songs enrichParams = injectFree (EnrichedSongResponse env songs enrichParams Pure)

fetchSongComments :: (SongQuery :<: f) => Env -> [UUID] -> Free f (Map UUID SongComment)
fetchSongComments env uuids = injectFree (FetchSongComments env uuids Pure)

fetchSongOpinions :: (SongQuery :<: f) => Env -> [UUID] -> Free f (Map UUID SongOpinion)
fetchSongOpinions env uuids = injectFree (FetchSongOpinions env uuids Pure)

fetchSongArtworks :: (SongQuery :<: f) => Env -> [UUID] -> Free f (Map UUID SongArtwork)
fetchSongArtworks env uuids = injectFree (FetchSongArtworks env uuids Pure)

searchSongs :: (SongQuery :<: f) => Env -> SearchInput -> SongSortOrder -> Limit -> Offset -> Free f (Map UUID Song, [UUID])
searchSongs env searchInput sortOrder fetchLimit offset = injectFree (SearchSongs env searchInput sortOrder fetchLimit offset Pure)

fetchSongContents :: (SongQuery :<: f) => Env -> [UUID] -> Free f (Map UUID SongContent)
fetchSongContents env uuids = injectFree (FetchSongContents env uuids Pure)

fetchSongArtists :: (SongQuery :<: f) => Env -> [UUID] -> Free f [(UUID, UUID, Text)]
fetchSongArtists env uuids = injectFree (FetchSongArtists env uuids Pure)
