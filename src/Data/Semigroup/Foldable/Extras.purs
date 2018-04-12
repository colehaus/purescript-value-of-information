module Data.Semigroup.Foldable.Extras where

import Prelude

import Data.Newtype (ala)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Semigroup.Foldable (class Foldable1, foldMap1)

maximum :: forall f a. Ord a => Foldable1 f => f a -> a
maximum = ala Max foldMap1

minimum :: forall f a. Ord a => Foldable1 f => f a -> a
minimum = ala Min foldMap1
