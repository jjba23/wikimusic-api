{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Env
  ( Env (..),
  )
where

import Data.Time
import Database.Beam.Sqlite (Connection)
import Hasql.Pool qualified
import Network.Wai.Logger (ApacheLogger)
import Optics
import Relude
import WikiMusic.Model.Config

data Env = Env
  { pool :: Hasql.Pool.Pool,
    cfg :: AppConfig,
    processStartedAt :: UTCTime,
    logger :: ApacheLogger,
    conn :: Connection,
    mailCss :: Text
  }

makeFieldLabelsNoPrefix ''Env
