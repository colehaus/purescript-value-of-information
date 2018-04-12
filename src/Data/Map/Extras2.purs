module Data.Map.Extras2 where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Map (Map, empty, unionWith)

unionsWith :: forall f k v. Ord k => Foldable f => (v -> v -> v) -> f (Map k v) -> Map k v
unionsWith f = foldl (unionWith f) empty
