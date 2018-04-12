module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Functor.Tagged (tagged, untagged)
import Math.Probability.Information.Value (valueOfInformation)
import Math.Probability.Information.Value.InvestigationAndDecisionTree.Internal (InvestigationAndDecisionTree)
import Math.Probability.Prob.Number (Prob)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Unsafe.Coerce (unsafeCoerce)


main :: Eff (QCRunnerEffects ()) Unit
main = do
  run [consoleReporter] do
    describe "expectedValueOfInformation" do
      it "works for trick coin" do
        shouldEqual (voi coinScenario) 0.5

voi ::
     forall finding choice p idt.
     Ord finding
  => Ord choice
  => InvestigationAndDecisionTree Prob finding choice Number
  -> Number
voi = untagged <<< valueOfInformation tagged (unsafeCoerce unit) (unsafeCoerce unit)

coinScenario :: forall idt p. idt p String String Number
coinScenario = unsafeCoerce unit
