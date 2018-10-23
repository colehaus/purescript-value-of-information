module Test.Main where

import Prelude

import Data.BigInt (BigInt)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.List as List
import Data.Map as Map
import Data.NonEmpty.Indexed as Indexed
import Data.Ratio (Ratio, (%))
import Data.Set as Set
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Math.Probability.Prob.BigInt (Prob(..))
import Test.QuickCheck (Result, (===), (>=?), (>?))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Math.Probability.Information.Value
  ( Value
  , evMaxDecide
  , evMaxValue
  , forget
  , investigationAndDecisionTreeDecide
  , investigationAndDecisionTreeValue
  , perfect
  , simpleDecisionTreeValue
  , valueOfInformation
  )
import Math.Probability.Information.Value.InvestigationAndDecisionTree
  ( InvestigationAndDecisionTree
  )
import Math.Probability.Information.Value.InvestigationAndDecisionTree as IDT
import Math.Probability.Information.Value.Utility (unsafeFromRightBecause)
import Test.Gen (b, genStringNumTree)


type Rational = Ratio BigInt
type Outcome = Rational

lff = List.fromFoldable
i = b 1 % b 1
o = b 0 % b 1
investigationAndDecisionTreeValue' =
  investigationAndDecisionTreeValue tagged evMaxDecide' evMaxValue'
simpleDecisionTreeValue' =
  simpleDecisionTreeValue tagged evMaxDecide' evMaxValue'
evMaxDecide' = evMaxDecide tagged probToValue
evMaxValue' = evMaxValue probToValue

probToValue :: Prob -> Tagged Value Outcome
probToValue (MkProb p) = tagged p

main :: Effect Unit
main = do
  run [consoleReporter] do
    describe "expectedValueOfInformation" do
      it "is never negative" $ quickCheck $ evoiNonNeg <$> genStringNumTree
      it "is strictly positive iff action changes" do
        quickCheck $ evoiChangeIffPositive <$> genStringNumTree
      it
        "produces less value for imperfect information than perfect information" do
        quickCheck $ evoiPerfectBest <$> genStringNumTree
      it "round trips via `forget <<< perfect`" do
        quickCheck $ perfectForgetRoundTrip <$> genStringNumTree
      it "works for trick coin" do
        shouldEqual (voi coinScenario) (b 1 % b 2)

voi ::
     forall finding choice.
     Ord finding
  => Ord choice
  => InvestigationAndDecisionTree Prob finding choice Outcome
  -> Rational
voi = untagged <<< valueOfInformation tagged evMaxDecide' evMaxValue'

evoiNonNeg :: InvestigationAndDecisionTree Prob String String Outcome -> Result
evoiNonNeg tree = voi tree >=? (b 0 % b 1)

evoiChangeIffPositive ::
     InvestigationAndDecisionTree Prob String String Outcome -> Result
evoiChangeIffPositive tree =
  if infoChoices == simpleChoice
    then voi tree === o
    else voi tree >? o
  where
    simpleChoice = Set.singleton <<< evMaxDecide' <<< forget $ tree
    infoChoices =
      Set.fromFoldable <<<
      Map.values <<<
      Indexed.fromNonEmpty Map.insert <<<
      investigationAndDecisionTreeDecide evMaxDecide' $
      tree

evoiPerfectBest :: InvestigationAndDecisionTree Prob String String Outcome -> Result
evoiPerfectBest tree = voi (perfect $ forget tree) >=? voi tree

perfectForgetRoundTrip :: InvestigationAndDecisionTree Prob String String Outcome -> Result
perfectForgetRoundTrip tree = forget tree === (forget <<< perfect <<< forget $ tree)

coinScenario :: InvestigationAndDecisionTree Prob String String Outcome
coinScenario =
  unsafeFromRightBecause "coinScenario" <<< IDT.fromElementary $
  lff
    [ Tuple
        (Tuple
           "coin is double heads"
           (lff
              [ Tuple "bet heads" (lff [Tuple i top, Tuple o bottom])
              , Tuple "bet tails" (lff [Tuple o top, Tuple i bottom])
              ]))
        (MkProb $ b 1 % b 2)
    , Tuple
        (Tuple
           "coin is double tails"
           (lff
              [ Tuple "bet heads" (lff [Tuple i bottom, Tuple o top])
              , Tuple "bet tails" (lff [Tuple o bottom, Tuple i top])
              ]))
        (MkProb $ b 1 % b 2)
    ]
