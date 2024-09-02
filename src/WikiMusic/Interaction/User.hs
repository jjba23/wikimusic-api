{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.User
  ( makeResetPasswordLinkAction,
    doPasswordResetAction,
    deleteUserAction,
    inviteUserAction,
  )
where

import Data.Text (pack, unpack)
import NeatInterpolation
import Network.HTTP.Base qualified
import Relude
import WikiMusic.Free.MailCommand
import WikiMusic.Free.UserCommand as UC
import WikiMusic.Free.UserQuery as UQ
import WikiMusic.Interaction.Model.User
import WikiMusic.Model.Mail
import WikiMusic.Protolude

makeResetPasswordLinkAction ::
  (UserCommand :<: f, MailCommand :<: f) =>
  Env ->
  Text ->
  Free f (Either UserError MakeResetPasswordLinkResponse)
makeResetPasswordLinkAction env userEmail = do
  maybeToken <- makeResetPasswordLink env (UC.UserEmail userEmail)
  doSendMailFromResetToken env maybeToken userEmail

doPasswordResetAction :: (UserCommand :<: f, UserQuery :<: f, MailCommand :<: f) => Env -> DoPasswordResetRequest -> Free f (Either UserError ())
doPasswordResetAction env req
  | (req ^. #password) == (req ^. #passwordConfirm) = do
      isTokenMatch <- doesTokenMatchByEmail env (UQ.UserEmail $ req ^. #email) (UserToken $ req ^. #token)
      case isTokenMatch of
        Left e -> pure . Left . SomeError . pack . show $ e
        Right doesMatch -> whenTokenMatches env req doesMatch
  | otherwise = pure . Left . SomeError $ "Passwords must match!"

whenTokenMatches ::
  (UserCommand :<: f) =>
  Env ->
  DoPasswordResetRequest ->
  Bool ->
  Free f (Either UserError ())
whenTokenMatches _ _ False = pure . Left $ AccessUnauthorizedError
whenTokenMatches env req True = do
  hasChangedPass <- changePasswordByEmail env (UC.UserEmail $ req ^. #email) (UC.UserPassword $ req ^. #password)
  case hasChangedPass of
    Left e -> pure . Left . SomeError . pack . show $ e
    Right _ -> do
      hasInvalidatedToken <- invalidateResetTokenByEmail env (UC.UserEmail $ req ^. #email)
      pure $ first (SomeError . pack . show) hasInvalidatedToken

deleteUserAction ::
  (UserCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  DeleteUsersRequest ->
  Free f (Either UserError ())
deleteUserAction env authUser req = do
  doWithRoles' authUser isAtLeastSuperUser AccessUnauthorizedError $ do
    s <- deleteUser env authUser (UC.UserEmail $ req ^. #email)
    pure $ first (SomeError . pack . show) s

inviteUserAction ::
  (UserCommand :<: f, MailCommand :<: f) =>
  Env ->
  WikiMusicUser ->
  InviteUsersRequest ->
  Free f (Either UserError MakeResetPasswordLinkResponse)
inviteUserAction env authUser req = do
  doWithRoles' authUser isAtLeastMaintainer AccessUnauthorizedError $ do
    maybeToken <- inviteUser env authUser (UC.UserEmail $ req ^. #email) (UC.UserName $ req ^. #displayName) (req ^. #role) (req ^. #description)
    doSendMailFromResetToken env maybeToken (req ^. #email)

doSendMailFromResetToken ::
  (UserCommand :<: f, MailCommand :<: f) =>
  Env ->
  Either UserCommandError Text ->
  Text ->
  Free f (Either UserError MakeResetPasswordLinkResponse)
doSendMailFromResetToken env maybeToken userEmail = do
  case maybeToken of
    Left e -> pure . Left . SomeError . pack . show $ e
    Right token -> do
      let mailCss = env ^. #mailCss
      let resetLink =
            (env ^. #cfg % #webFrontend % #baseUrl)
              <> "/passwords/do-reset?"
              <> (pack . Network.HTTP.Base.urlEncodeVars $ [("token", unpack token)])
          mailBody =
            [trimming|
                       <style>
                       $mailCss
                       </style>
                       <h1>Reset your password on WikiMusic</h1>                  
                       <p>We have received a request to reset the password for your user on WikiMusic.</p>
                       <a href="$resetLink">Go reset your password on WikiMusic!</a>
                       <p>Feel free to ignore this e-mail if you did not request it</p>
                       <br/>
                       <small>If you had trouble clicking that link, please manually copy paste it:<br/>$resetLink</small>
                     |]
      mailR <- sendMail env (MailSendRequest {subject = "WikiMusic - Reset Password", email = userEmail, name = Nothing, body = mailBody})
      case mailR of
        Left e -> pure . Left . SomeError . pack . show . NotificationError . pack . show $ e
        Right _ -> pure . Right $ MakeResetPasswordLinkResponse {user = userEmail}
