{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Servant.Utilities
  ( err204,
    loginRoute,
    authCheckIO,
    maybe200,
    maybe204,
    systemInformationRoute,
    doWithAuth,
  )
where

import Data.Password.Bcrypt
import Data.Text qualified as T
import Servant as S
import WikiMusic.Free.AuthQuery
import WikiMusic.Model.Auth
import WikiMusic.Model.Env
import WikiMusic.Model.Other
import WikiMusic.Protolude
import WikiMusic.Sqlite.AuthQuery ()

loginRoute ::
  Env ->
  LoginRequest ->
  Handler
    ( Headers
        '[ S.Header "x-wikimusic-auth" Text
         ]
        NoContent
    )
loginRoute env (LoginRequest inputEmail inputPassword) = do
  eitherWikiMusicUser <- liftIO (exec @AuthQuery $ fetchUserForAuthCheck env (T.pack inputEmail))
  either
    (const $ throwError err401)
    (`verifyUserLogin` inputPassword)
    eitherWikiMusicUser

verifyUserLogin ::
  Maybe WikiMusicUser ->
  String ->
  Handler
    ( Headers
        '[ S.Header "x-wikimusic-auth" Text
         ]
        NoContent
    )
verifyUserLogin Nothing _ = throwError err401
verifyUserLogin (Just wikimusicUser) inputPassword = do
  doAfterPasswordCheck wikimusicUser passwordCheckResult
  where
    inputPass = mkPassword (T.pack inputPassword)
    passwordCheckResult =
      maybe
        PasswordCheckFail
        (checkPassword inputPass . PasswordHash)
        (wikimusicUser ^. #passwordHash)

doAfterPasswordCheck ::
  WikiMusicUser ->
  PasswordCheck ->
  Handler
    ( Headers
        '[ S.Header "x-wikimusic-auth" Text
         ]
        NoContent
    )
doAfterPasswordCheck _ PasswordCheckFail = throwError err401
doAfterPasswordCheck wikimusicUser' PasswordCheckSuccess = do
  let tok = fromMaybe "" (wikimusicUser' ^. #authToken)
  throwError
    $ ServerError
      { errHTTPCode = 204,
        errReasonPhrase = "No Content",
        errBody = "",
        errHeaders =
          [ ("x-wikimusic-auth", WikiMusic.Protolude.encodeUtf8 tok)
          ]
      }

systemInformationRoute ::
  Env ->
  Handler SystemInformationResponse
systemInformationRoute env = do
  pure
    SystemInformationResponse
      { reportedVersion = env ^. #cfg % #dev % #reportedVersion,
        processStartedAt = env ^. #processStartedAt
      }

authCheckIO ::
  Env ->
  Text ->
  IO (Maybe WikiMusicUser)
authCheckIO env token = do
  eitherWikiMusicUser <- liftIO (exec @AuthQuery $ fetchUserFromToken env token)
  case eitherWikiMusicUser of
    Left _ -> do
      pure Nothing
    Right maybeWikiMusicUser -> do
      case maybeWikiMusicUser of
        Nothing -> pure Nothing
        Just u -> do
          pure $ Just u

err204 :: ServerError
err204 =
  ServerError
    { errHTTPCode = 204,
      errReasonPhrase = "No Content",
      errBody = "",
      errHeaders = []
    }

maybe204 :: (Show s) => Either s b -> Handler b
maybe204 (Left err) =
  throwError
    $ err500
      { errBody = fromString . WikiMusic.Protolude.show $ err
      }
maybe204 _ = throwError err204

maybe200 :: (Show s) => Either s b -> Handler b
maybe200 (Left err) =
  throwError
    $ err500
      { errBody = fromString . WikiMusic.Protolude.show $ err
      }
maybe200 (Right x) = pure x

doWithAuth :: Env -> Maybe Text -> (WikiMusicUser -> Handler a) -> Handler a
doWithAuth env authToken eff = do
  case authToken of
    Nothing -> throwError err401
    Just "" -> throwError err401
    Just t -> do
      authUser <- liftIO $ authCheckIO env t
      case authUser of
        Nothing -> throwError err401
        Just auth -> eff auth
