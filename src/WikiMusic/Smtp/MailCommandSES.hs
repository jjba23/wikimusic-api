{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Smtp.MailCommandSES () where

import Data.Text qualified as T
import Network.Mail.Mime hiding (simpleMail)
import Network.Mail.Smtp hiding (htmlPart)
import Optics
import Relude
import System.Timeout
import WikiMusic.Free.MailCommand
import WikiMusic.Model.Env
import WikiMusic.Model.Mail
import WikiMusic.Protolude

instance Exec MailCommand where
  execAlgebra (SendMail env req next) = mailSend env req >>= next

mailSend :: (MonadIO m) => Env -> MailSendRequest -> m (Either MailCommandError MailCommandOutcome)
mailSend env req = do
  mailSendingResult <- liftIO $ timeout timeoutSeconds doSendMail
  case mailSendingResult of
    Nothing -> pure . Left $ MailError ""
    Just _ -> pure $ Right MailSent
  where
    mailCfg = env ^. #cfg % #mail
    timeoutSeconds = (mailCfg ^. #sendTimeoutSeconds) * 1000000
    preparedMail =
      simpleMail
        (Address (Just (mailCfg ^. #senderName)) (mailCfg ^. #senderMail))
        [Address (req ^. #name) (req ^. #email)] -- to
        [] -- cc
        [] -- bcc
        (req ^. #subject)
        [ htmlPart (fromString . T.unpack $ req ^. #body)
        ] -- body
    doSendMail =
      sendMailWithLoginSTARTTLS
        (T.unpack $ mailCfg ^. #host)
        (maybe "" T.unpack $ mailCfg ^. #user)
        (maybe "" T.unpack $ mailCfg ^. #password)
        preparedMail
