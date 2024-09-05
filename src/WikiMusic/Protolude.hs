module WikiMusic.Protolude
  ( module Relude,
    module Data.UUID,
    module Optics,
    module WikiMusic.Model.Env,
    module WikiMusic.Model.Auth,
    module Data.Time,
    module Data.UUID.V4,
    module Contravariant.Extras.Contrazip,
    module Free.AlaCarte,
    mapMap,
    filterMap,
    filterText,
    module NeatInterpolation,
  )
where

import Contravariant.Extras.Contrazip
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), ZonedTime, getCurrentTime, secondsToDiffTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Free.AlaCarte
import NeatInterpolation
import Optics hiding (uncons)
import Relude
import WikiMusic.Model.Auth hiding (show)
import WikiMusic.Model.Env

mapMap :: (a -> b) -> Map k a -> Map k b
mapMap = Map.map

filterMap :: (a -> Bool) -> Map k a -> Map k a
filterMap = Map.filter

filterText :: (Char -> Bool) -> Text -> Text
filterText = T.filter
