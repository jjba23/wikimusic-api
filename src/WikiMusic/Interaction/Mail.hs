{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.Mail (sendMailAction) where

import WikiMusic.Free.MailCommand
import WikiMusic.Model.Mail
import WikiMusic.Protolude

sendMailAction ::
  (MailCommand :<: f) =>
  Env ->
  MailSendRequest ->
  Free f (Either MailCommandError MailCommandOutcome)
sendMailAction = sendMail
