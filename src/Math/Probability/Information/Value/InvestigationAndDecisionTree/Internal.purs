module Math.Probability.Information.Value.InvestigationAndDecisionTree.Internal where

import Prelude

import Data.Tuple (Tuple)
import Math.Probability.Dist (Dist)

import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree)

newtype InvestigationAndDecisionTree prob finding choice result =
  MkInvestigationAndDecisionTree (Dist prob (Tuple finding (SimpleDecisionTree prob choice result)))

derive newtype instance showIDT ::
     (Ord choice, Show finding, Show choice, Show result, Show prob)
  => Show (InvestigationAndDecisionTree prob finding choice result)

derive newtype instance eqIDT ::
     (Ord finding, Ord choice, Ord result, Ord prob, Semiring prob)
  => Eq (InvestigationAndDecisionTree prob finding choice result)

derive newtype instance ordIDT ::
     (Ord finding, Ord choice, Ord result, Ord prob, Semiring prob)
  => Ord (InvestigationAndDecisionTree prob finding choice result)
