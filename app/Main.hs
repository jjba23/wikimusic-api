module Main (main) where

import WikiMusic.Boot qualified
import WikiMusic.Protolude

main :: (MonadIO m) => m ()
main = liftIO $ WikiMusic.Boot.boot
