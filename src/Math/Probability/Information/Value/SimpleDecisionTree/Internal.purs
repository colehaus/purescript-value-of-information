module Math.Probability.Information.Value.SimpleDecisionTree.Internal where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty.Indexed (NonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Math.Probability.Dist (Dist)

newtype SimpleDecisionTree prob choice result =
  MkSimpleDecisionTree (NonEmpty Map choice (Dist prob result))

instance showSDT ::
     (Ord choice, Show choice, Show result, Show prob)
  => Show (SimpleDecisionTree prob choice result) where
  show (MkSimpleDecisionTree tree) =
    "(MkSimpleDecisionTree " <> show (Indexed.fromNonEmpty Map.insert tree) <> ")"

derive newtype instance eqSDT ::
     (Eq choice, Ord result, Eq prob, Semiring prob)
  => Eq (SimpleDecisionTree prob choice result)

derive newtype instance ordSDT ::
     (Ord choice, Ord result, Ord prob, Semiring prob)
  => Ord (SimpleDecisionTree prob choice result)
