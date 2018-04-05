module Math.Probability.Information.Value.Internal where

import Data.Map (Map)
import Data.NonEmpty (NonEmpty)
import Data.Tuple (Tuple)
import Math.Probability (Dist)

type SimpleDecisionTree choice result = NonEmpty (Map choice) (Dist result)
newtype InvestigationAndDecisionTree finding choice result =
  MkInvestigationAndDecisionTree (Dist (Tuple finding (SimpleDecisionTree choice result)))
