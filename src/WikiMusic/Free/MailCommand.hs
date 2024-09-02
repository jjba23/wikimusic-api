{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Free.MailCommand
  ( sendMail,
    MailCommand (..),
  )
where

import WikiMusic.Model.Env
import WikiMusic.Model.Mail
import WikiMusic.Protolude

data MailCommand a
  = SendMail Env MailSendRequest (Either MailCommandError MailCommandOutcome -> a)
  deriving (Functor)

sendMail :: (MailCommand :<: f) => Env -> MailSendRequest -> Free f (Either MailCommandError MailCommandOutcome)
sendMail env req = injectFree (SendMail env req Pure)
