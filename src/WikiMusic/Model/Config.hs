{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Config where

import Optics
import Relude
import Toml

data SqliteConfig = SqliteConfig
  { path :: Text,
    runMigrations :: Bool
  }
  deriving (Generic, Eq, Show)

sqliteConfigCodec :: TomlCodec SqliteConfig
sqliteConfigCodec =
  SqliteConfig
    <$> Toml.text "path"
    .= (^. #path)
    <*> Toml.bool "run-migrations"
    .= (^. #runMigrations)

data ServantConfig = ServantConfig
  { port :: Int,
    host :: Text
  }
  deriving (Generic, Eq, Show)

servantConfigCodec :: TomlCodec ServantConfig
servantConfigCodec =
  ServantConfig
    <$> Toml.int "port"
    .= (^. #port)
    <*> Toml.text "host"
    .= (^. #host)

data CorsConfig = CorsConfig
  { origins :: [Text],
    methods :: [Text],
    requestHeaders :: [Text]
  }
  deriving (Generic, Eq, Show)

corsConfigCodec :: TomlCodec CorsConfig
corsConfigCodec =
  CorsConfig
    <$> Toml.arrayOf Toml._Text "origins"
    .= (^. #origins)
    <*> Toml.arrayOf Toml._Text "methods"
    .= (^. #methods)
    <*> Toml.arrayOf Toml._Text "request-headers"
    .= (^. #requestHeaders)

data MailConfig = MailConfig
  { sendTimeoutSeconds :: Int,
    host :: Text,
    userFile :: Text,
    user :: Maybe Text,
    passwordFile :: Text,
    password :: Maybe Text,
    senderName :: Text,
    senderMail :: Text
  }
  deriving (Generic, Eq, Show)

mailConfigCodec :: TomlCodec MailConfig
mailConfigCodec =
  MailConfig
    <$> Toml.int "send-timeout-seconds"
    .= (^. #sendTimeoutSeconds)
    <*> Toml.text "host"
    .= (^. #host)
    <*> Toml.text "user-file"
    .= (^. #userFile)
    <*> Toml.dioptional (Toml.text "user")
    .= (^. #user)
    <*> Toml.text "password-file"
    .= (^. #passwordFile)
    <*> Toml.dioptional (Toml.text "password")
    .= (^. #password)
    <*> Toml.text "sender-name"
    .= (^. #senderName)
    <*> Toml.text "sender-mail"
    .= (^. #senderMail)

newtype WebFrontendConfig = WebFrontendConfig
  { baseUrl :: Text
  }
  deriving (Generic, Eq, Show)

webFrontendConfigCodec :: TomlCodec WebFrontendConfig
webFrontendConfigCodec =
  WebFrontendConfig
    <$> Toml.text "base-url"
    .= (^. #baseUrl)

newtype DevConfig = DevConfig
  { reportedVersion :: Text
  }
  deriving (Generic, Eq, Show)

devCodec :: TomlCodec DevConfig
devCodec = DevConfig <$> Toml.text "reported-version" .= (^. #reportedVersion)

data AppConfig = AppConfig
  { servant :: ServantConfig,
    sqlite :: SqliteConfig,
    cors :: CorsConfig,
    mail :: MailConfig,
    webFrontend :: WebFrontendConfig,
    dev :: DevConfig
  }
  deriving (Generic, Eq, Show)

appConfigCodec :: TomlCodec AppConfig
appConfigCodec =
  AppConfig
    <$> Toml.table servantConfigCodec "servant"
    .= (^. #servant)
    <*> Toml.table sqliteConfigCodec "sqlite"
    .= (^. #sqlite)
    <*> Toml.table corsConfigCodec "cors"
    .= (^. #cors)
    <*> Toml.table mailConfigCodec "mail"
    .= (^. #mail)
    <*> Toml.table webFrontendConfigCodec "web-frontend"
    .= (^. #webFrontend)
    <*> Toml.table devCodec "dev"
    .= (^. #dev)

makeFieldLabelsNoPrefix ''AppConfig
makeFieldLabelsNoPrefix ''SqliteConfig
makeFieldLabelsNoPrefix ''ServantConfig
makeFieldLabelsNoPrefix ''CorsConfig
makeFieldLabelsNoPrefix ''MailConfig
makeFieldLabelsNoPrefix ''WebFrontendConfig
makeFieldLabelsNoPrefix ''DevConfig
