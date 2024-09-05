{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Config (readConfig) where

import Data.ByteString.Lazy qualified as BL
import Data.Text (strip, unpack)
import Data.Text.Encoding qualified
import Toml
import WikiMusic.Model.Config
import WikiMusic.Protolude

readConfig :: (MonadIO m) => Text -> m (Either Text AppConfig)
readConfig filePath = do
  parseResult <- liftIO $ decodeFileEither appConfigCodec (unpack filePath)
  case parseResult of
    Left e -> pure . Left $ prettyTomlDecodeErrors e
    Right r -> readSecrets r <&> Right

readSecrets :: (MonadIO m) => AppConfig -> m AppConfig
readSecrets cfg = do
  mailPassword <- readSecretFromFile cfg (^. #mail % #passwordFile)
  mailUser <- readSecretFromFile cfg (^. #mail % #userFile)
  let 
      withMailPasswordCfg =
        cfg
          & #mail
          .~ ((withPostgresCfg ^. #mail) & #password ?~ mailPassword)
      withMailUserCfg =
        withMailPasswordCfg
          & #mail
          .~ ((withPostgresCfg ^. #mail) & #user ?~ mailUser)
  pure withMailUserCfg

readSecretFromFile :: (MonadIO m) => t -> (t -> Text) -> m Text
readSecretFromFile cfg getFilePath = do
  passwordFileContents <- liftIO . BL.readFile . unpack . getFilePath $ cfg
  pure
    . strip
    . Data.Text.Encoding.decodeUtf8
    . BL.toStrict
    $ passwordFileContents
