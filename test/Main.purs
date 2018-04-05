module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.QuickCheck (arbitrary, (/==), (===))
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Math" do
    it "works" $
      quickCheck $ do
        n <- arbitrary
        pure $ (n * 2 / 2) === n
    it "works again" $ do
      quickCheck $ do
        n <- arbitrary
        pure $ ((n + 1) * 2) /== n
