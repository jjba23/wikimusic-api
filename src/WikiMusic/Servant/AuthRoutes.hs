{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Servant.AuthRoutes
  ( fetchMeRoute,
  )
where

import Servant
import WikiMusic.Free.AuthQuery
import WikiMusic.Interaction.Auth
import WikiMusic.Interaction.Model.Auth
import WikiMusic.Model.Env
import WikiMusic.PostgreSQL.AuthQuery ()
import WikiMusic.Protolude
import WikiMusic.Servant.Utilities

fetchMeRoute :: Env -> Maybe Text -> Handler GetMeQueryResponse
fetchMeRoute env authToken = do
  doWithAuth
    env
    authToken
    ( \authUser -> do
        maybeUser <- liftIO (exec @AuthQuery $ fetchMeAction env (authUser ^. #identifier))
        maybe (throwError err404) pure maybeUser
    )
