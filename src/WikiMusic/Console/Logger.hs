{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Console.Logger () where

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Time
import Relude
import WikiMusic.Free.Logger
import WikiMusic.Protolude

instance Exec Logger where
  execAlgebra (LogInfo msg next) = do
    now <- iso8601
    _ <- textToStdout (logExpr now "INFO" msg)
    next
  execAlgebra (LogError msg next) = do
    now <- iso8601
    _ <- textToStderr (logExpr now "ERROR" msg)
    next
  execAlgebra (LogDebug msg next) = do
    now <- iso8601
    _ <- textToStdout (logExpr now "DEBUG" msg)
    next

textToStdout :: (MonadIO m) => Text -> m ()
textToStdout = liftIO . BL.hPutStr stdout . BL.fromStrict . encodeUtf8 . liner

textToStderr :: (MonadIO m) => Text -> m ()
textToStderr = liftIO . BL.hPutStr stderr . BL.fromStrict . encodeUtf8 . liner

liner :: Text -> Text
liner = (<> "\n")

-- Construct format string according to <http://en.wikipedia.org/wiki/ISO_8601 ISO-8601>.
iso8601 :: (MonadIO m) => m Text
iso8601 = do
  n <- liftIO getCurrentTime
  let n' = T.pack $ formatTime defaultTimeLocale (T.unpack "%Y-%m-%dT%H:%M:%SZ") n
  pure n'

logExpr :: (Semigroup a, IsString a) => a -> a -> a -> a
logExpr now lev msg = "[" <> now <> "][" <> lev <> "] " <> msg
