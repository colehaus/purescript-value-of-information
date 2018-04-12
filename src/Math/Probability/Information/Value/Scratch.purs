module Math.Probability.Information.Value.Scratch where

import Prelude

import Data.BigInt (BigInt, fromInt)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty.Indexed as Indexed
import Data.Profunctor.Strong (first)
import Data.Rational ((%))
import Data.Tuple (Tuple(Tuple))
import Math.Probability.Information.Value (forget, perfect)
import Math.Probability.Information.Value.InvestigationAndDecisionTree (InvestigationAndDecisionTree)
import Math.Probability.Information.Value.InvestigationAndDecisionTree as IDT
import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree)
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility (unsafeFromRightBecause)
import Math.Probability.Prob.BigInt (Prob(..))

lff :: forall a. Array a -> List a
lff = List.fromFoldable

b :: Int -> BigInt
b = fromInt

x :: SimpleDecisionTree Prob String String
x =
  unsafeFromRightBecause "x" <<< SDT.fromElementary $
  lff
    [ Tuple "a" (lff [Tuple "c" (MkProb $ b 2 % b 10), Tuple "d" (MkProb $ b 8 % b 10)])
    , Tuple "b" (lff [Tuple "e" (MkProb $ b 3 % b 10), Tuple "f" (MkProb $ b 7 % b 10)])
    ]

y :: InvestigationAndDecisionTree Prob (Map String String) String String
y =
  unsafeFromRightBecause "y" $
  IDT.lift (map (first (Indexed.fromNonEmpty Map.insert))) $ perfect x

z :: SimpleDecisionTree Prob String String
z = forget y
