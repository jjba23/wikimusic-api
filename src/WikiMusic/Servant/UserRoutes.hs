{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Servant.UserRoutes
  ( makeResetPasswordLinkRoute,
    doPasswordResetRoute,
    inviteUserRoute,
    deleteUserRoute,
  )
where

import Servant
import WikiMusic.Free.MailCommand
import WikiMusic.Free.UserCommand
import WikiMusic.Free.UserQuery
import WikiMusic.Interaction.Model.User
import WikiMusic.Interaction.User
import WikiMusic.Model.Env
import WikiMusic.PostgreSQL.UserCommand ()
import WikiMusic.PostgreSQL.UserQuery ()
import WikiMusic.Protolude
import WikiMusic.SMTP.MailCommandSES ()
import WikiMusic.Servant.Utilities

makeResetPasswordLinkRoute :: Env -> Text -> Handler MakeResetPasswordLinkResponse
makeResetPasswordLinkRoute env userEmail = liftIO (exec @(UserCommand :+: MailCommand) $ makeResetPasswordLinkAction env userEmail) >>= maybe200

doPasswordResetRoute :: Env -> DoPasswordResetRequest -> Handler ()
doPasswordResetRoute env req = liftIO (exec @(UserCommand :+: UserQuery :+: MailCommand) $ doPasswordResetAction env req) >>= maybe204

inviteUserRoute :: Env -> Maybe Text -> InviteUsersRequest -> Handler MakeResetPasswordLinkResponse
inviteUserRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @(UserCommand :+: MailCommand) $ inviteUserAction env authUser req) >>= maybe200
    )

deleteUserRoute :: Env -> Maybe Text -> DeleteUsersRequest -> Handler ()
deleteUserRoute env authToken req =
  doWithAuth
    env
    authToken
    ( \authUser ->
        liftIO (exec @UserCommand $ deleteUserAction env authUser req) >>= maybe204
    )
