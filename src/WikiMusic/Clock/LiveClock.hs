{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Clock.LiveClock () where

import Data.Text qualified as T
import Data.Time
import Free.AlaCarte
import Relude
import WikiMusic.Free.Clock

instance Exec Clock where
  execAlgebra (TimeElapsedUntilNow fromTime f) = do
    diffWithNow fromTime >>= f
  execAlgebra (Now f) = do
    getCurrentTime >>= f

diffWithNow :: (MonadIO m) => UTCTime -> m Text
diffWithNow fromTime = do
  now' <- liftIO getCurrentTime
  pure $ T.pack . show $ diffUTCTime now' fromTime
