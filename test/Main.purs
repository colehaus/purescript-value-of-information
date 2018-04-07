module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Math.Probability.Information.Value (InvestigationAndDecisionTree)
import Test.QuickCheck (arbitrary, Result)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Unsafe.Coerce (unsafeCoerce)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "expectedValueOfInformation" do
    it "is never negative" $
      quickCheck $ evoiNonNeg <$> genStringNumBeforeAndAfterInfo
    it "is strictly positive iff action changes" do
      quickCheck $ evoiChangeIffPositive <$> genStringNumBeforeAndAfterInfo
    it "produces less value for imperfect information than perfect information" do
      quickCheck $ evoiPerfectBest <$> genPerfectImperfectPair

genBeforeAndAfterInfo ::
  forall finding choice result.
  Gen finding -> Gen choice -> Gen result ->
  Gen (InvestigationAndDecisionTree finding choice result)
genBeforeAndAfterInfo genA genB = unsafeCoerce unit

genStringNumBeforeAndAfterInfo :: Gen (InvestigationAndDecisionTree String String Number)
genStringNumBeforeAndAfterInfo = genBeforeAndAfterInfo arbitrary arbitrary arbitrary

genPerfectImperfectPair :: Gen { perfect :: InvestigationAndDecisionTree String String Number, imperfect :: InvestigationAndDecisionTree String String Number }
genPerfectImperfectPair = unsafeCoerce unit

evoiNonNeg :: InvestigationAndDecisionTree String String Number -> Result
evoiNonNeg bAndA = unsafeCoerce unit

evoiChangeIffPositive :: InvestigationAndDecisionTree String String Number -> Result
evoiChangeIffPositive bAndA = unsafeCoerce unit

evoiPerfectBest :: { perfect :: InvestigationAndDecisionTree String String Number, imperfect :: InvestigationAndDecisionTree String String Number } -> Result
evoiPerfectBest { perfect, imperfect } = unsafeCoerce unit
