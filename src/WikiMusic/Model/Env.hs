{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Env
  ( Env (..),
  )
where

import Data.Time
import Database.Beam.Sqlite.Connection
import Database.SQLite.Simple
import Network.Wai.Logger (ApacheLogger)
import Optics
import Relude
import WikiMusic.Model.Config

data Env = Env
  { cfg :: AppConfig,
    processStartedAt :: UTCTime,
    logger :: ApacheLogger,
    conn :: Connection,
    mailCss :: Text
  }

makeFieldLabelsNoPrefix ''Env
