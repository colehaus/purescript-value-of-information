module Math.Probability.Information.Value.InvestigationAndDecisionTree.Internal where

import Prelude

import Data.Tuple.Nested (Tuple4)

newtype InvestigationAndDecisionTree prob finding choice result =
  MkInvestigationAndDecisionTree (Tuple4 prob finding choice result)
