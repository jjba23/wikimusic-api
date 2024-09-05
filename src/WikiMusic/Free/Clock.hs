module WikiMusic.Free.Clock
  ( timeElapsedUntilNow,
    now,
    Clock (..),
  )
where

import Data.Time
import Free.AlaCarte
import Relude

type Clock :: Type -> Type
data Clock a
  = TimeElapsedUntilNow UTCTime (Text -> a)
  | Now (UTCTime -> a)
  deriving (Functor)

timeElapsedUntilNow :: (Clock :<: f) => UTCTime -> Free f Text
timeElapsedUntilNow fromTime = injectFree (TimeElapsedUntilNow fromTime Pure)

now :: (Clock :<: f) => Free f UTCTime
now = injectFree (Now Pure)
