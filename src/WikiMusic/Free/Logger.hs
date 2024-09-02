module WikiMusic.Free.Logger
  ( logInfo,
    logError,
    logDebug,
    Logger (..),
  )
where

import Free.AlaCarte
import Relude

type Logger :: Type -> Type
data Logger a
  = LogInfo Text a
  | LogError Text a
  | LogDebug Text a
  deriving (Functor)

logInfo :: (Logger :<: f) => Text -> Free f ()
logInfo message = injectFree (LogInfo message (Pure ()))

logError :: (Logger :<: f) => Text -> Free f ()
logError message = injectFree (LogError message (Pure ()))

logDebug :: (Logger :<: f) => Text -> Free f ()
logDebug message = injectFree (LogDebug message (Pure ()))
