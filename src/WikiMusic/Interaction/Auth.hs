{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.Auth
  ( fetchMeAction,
  )
where

import WikiMusic.Free.AuthQuery
import WikiMusic.Interaction.Model.Auth
import WikiMusic.Protolude
import WikiMusic.Sqlite.AuthQuery ()

fetchMeAction :: (AuthQuery :<: f) => Env -> UUID -> Free f (Maybe GetMeQueryResponse)
fetchMeAction env identifier = do
  maybeUserOrErr <- fetchMe env identifier
  case maybeUserOrErr of
    Left _ -> do
      pure Nothing
    Right maybeWikiMusicUser -> do
      pure $ userToResponse <$> maybeWikiMusicUser
  where
    userToResponse x =
      GetMeQueryResponse
        { identifier = x ^. #identifier,
          displayName = x ^. #displayName,
          emailAddress = x ^. #emailAddress,
          roles = x ^. #roles
        }
