module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (either)
import Data.Foldable (length)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (un)
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.NonEmpty.Extras (hmap)
import Data.NonEmpty.Indexed (deindex, index)
import Data.NonEmpty.Indexed as Indexed
import Data.Ratio (Ratio, denominator, numerator)
import Data.Rational (Rational, (%))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (range)
import Math.Probability.Dist as Dist
import Math.Probability.Dist.Internal (Dist)
import Math.Probability.Information.Value (Value, evMaxDecide, evMaxValue, forget, investigationAndDecisionTreeDecide, investigationAndDecisionTreeValue, perfect, simpleDecisionTreeValue, valueOfInformation)
import Math.Probability.Information.Value.InvestigationAndDecisionTree (InvestigationAndDecisionTree)
import Math.Probability.Information.Value.InvestigationAndDecisionTree as IDT
import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree)
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility (nonEmptyMap, nonEmptySet, nonEmptyToSet, unsafeFromJustBecause, unsafeFromRightBecause)
import Math.Probability.Prob.Number (Prob(..))
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (Result, arbitrary, (<?>), (===), (>=?))
import Test.QuickCheck.Data.AlphaNumString (AlphaNumString(..))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


main :: Eff (QCRunnerEffects ()) Unit
main = do
  -- log <<< show <<< map (forget <<< perfect <<< forget) =<< Gen.randomSample genStringNumTree
  run [consoleReporter] do
    describe "expectedValueOfInformation" do
      it "is never negative" $ quickCheck $ evoiNonNeg <$> genStringNumTree
      -- it "is strictly positive iff action changes" do
      --   quickCheck' 1 $ evoiChangeIffPositive <$> genStringNumTree
      it
        "produces less value for imperfect information than perfect information" do
        quickCheck' 10 $ evoiPerfectBest <$> genStringNumTree
      it "round trips via `forget <<< perfect`" do
        quickCheck' 10 $ perfectForgetRoundTrip <$> genStringNumTree
      it "works for trick coin" do
        shouldEqual (voi coinScenario) 0.5

voi ::
     forall finding choice p.
     Ord finding
  => Ord choice
  => InvestigationAndDecisionTree Prob finding choice DResult
  -> Number
voi = untagged <<< valueOfInformation tagged (evMaxDecide tagged probToValue) (evMaxValue probToValue)

probToValue :: Prob -> Tagged Value DResult
probToValue (MkProb p) = tagged p

rbn :: Ratio BigInt -> Number
rbn r = BigInt.toNumber (numerator r) / BigInt.toNumber (denominator r)

lff :: forall a. Array a -> List a
lff = List.fromFoldable

i :: Ratio BigInt
i = b 1 % b 1
o :: Ratio BigInt
o = b 0 % b 1

coinScenario ::
     InvestigationAndDecisionTree Prob String String DResult
coinScenario =
  unsafeFromRightBecause "coinScenario" <<< IDT.fromElementary $
  lff
    [ Tuple
        (Tuple
           "coins is double heads"
           (lff
              [ Tuple
                  "bet heads"
                  (lff
                     [ Tuple 1.0 top
                     , Tuple 0.0 bottom
                     ])
              , Tuple
                  "bet tails"
                  (lff
                     [ Tuple 0.0 top
                     , Tuple 1.0 bottom
                     ])
              ]))
        (MkProb $ 0.5)
    , Tuple
        (Tuple
           "coins is double tails"
           (lff
              [ Tuple
                  "bet heads"
                  (lff
                     [ Tuple 1.0 bottom
                     , Tuple 0.0 top
                     ])
              , Tuple
                  "bet tails"
                  (lff
                     [ Tuple 0.0 bottom
                     , Tuple 1.0 top
                     ])
              ]))
        (MkProb $ 0.5)
    ]

genInvestigationAndDecisionTree ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Show finding
  => Show choice
  => Show result
  => Show prob
  => EuclideanRing prob
  => Gen finding
  -> Gen choice
  -> Gen result
  -> Gen prob
  -> Gen (InvestigationAndDecisionTree prob finding choice result)
genInvestigationAndDecisionTree genFinding genChoice genResult genProb = do
  idtSkeleton <-
    genInvestigationAndDecisionTreeSkeleton genFinding genChoice genResult
  sdts <- traverse (genSimpleDecisionTree genProb) idtSkeleton
  idt <-
    genDist genProb (deindex id (Set.fromFoldable <<< asList <<< Map.toUnfoldable) sdts)
  pure <<<
    either (unsafeCrashWith <<< show <<< Tuple idt) id <<< IDT.make $
    idt
  where
    asList :: forall a. List a -> List a
    asList = id

genInvestigationAndDecisionTreeSkeleton ::
     forall finding choice result.
     Ord finding
  => Ord choice
  => Ord result
  => Gen finding
  -> Gen choice
  -> Gen result
  -> Gen (IDT.Skeleton finding choice result)
genInvestigationAndDecisionTreeSkeleton genFinding genChoice genResult = do
  findingSize <- Gen.chooseInt 1 4
  findings <- hmap List.fromFoldable <$> sizedSetOf1 findingSize genFinding
  sdtSkeleton <- genSimpleDecisionTreeSkeleton genChoice genResult
  pure $
    index
      (_ `Tuple` sdtSkeleton)
      (Map.fromFoldable <<< map (_ `Tuple` sdtSkeleton))
      findings

genSimpleDecisionTreeSkeleton ::
     forall choice result.
     Ord choice
  => Ord result
  => Gen choice
  -> Gen result
  -> Gen (SDT.Skeleton choice result)
genSimpleDecisionTreeSkeleton genChoice genResult = do
  choiceSize <- Gen.chooseInt 1 4
  choices <- sizedSetOf1 choiceSize genChoice
  results <- nonOverlappingSets (length choices * 4) (length choices) genResult
  pure <<<
    map (unsafeFromJustBecause reason <<< nonEmptySet) <<<
    unsafeFromJustBecause "`setOf`" <<< nonEmptyMap <<< Map.fromFoldable $
    List.zip (List.fromFoldable choices) (List.fromFoldable results)
  where
    reason = "Buckets are `NonEmpty` by construction"

nonOverlappingSets ::
     forall a.
     Ord a
  => Int
  -> Int
  -> Gen a
  -> Gen (Set (Set a))
nonOverlappingSets total numBuckets genA = bucket numBuckets =<< sizedSetOf total genA

-- TODO: Make inner `Set` `NonEmpty` once `instance Ord1 Set` lands
bucket ::
     forall a.
     Ord a
  => Int
  -> Set a
  -> Gen (Set (Set a))
bucket n as = do
  -- Want to ensure no bucket is empty
  as' <- map List.fromFoldable <<< Gen.shuffle <<< Array.fromFoldable $ as
  let bucketBase = List.zipWith ((_ <<< Set.singleton) <<< Tuple) (range 1 n) as'
  bucketPairs <-
    traverse (\a -> (_ `Tuple` Set.singleton a) <$> Gen.chooseInt 1 n) <<<
    List.drop n $
    as'
  pure <<<
    Set.fromFoldable <<<
    Map.values <<< Map.fromFoldableWith (<>) $
    bucketBase <> bucketPairs

setOf1 :: forall a. Ord a => Gen a -> Gen (NonEmpty Set a)
setOf1 genA = nonEmptyToSet <$> Gen.arrayOf1 genA

sizedSetOf1 ::
     forall a. Ord a
  => Int
  -> Gen a
  -> Gen (NonEmpty Set a)
sizedSetOf1 n genA = do
  set <- go Set.empty
  pure <<< unsafeFromJustBecause "n > 0" <<< nonEmptySet $ set
  where
    go s
      | Set.size s == n = pure s
      | otherwise = go <<< flip Set.insert s =<< genA

sizedSetOf ::
     forall a. Ord a
  => Int
  -> Gen a
  -> Gen (Set a)
sizedSetOf n genA = go Set.empty
  where
    go s
      | Set.size s == n = pure s
      | otherwise = go <<< flip Set.insert s =<< genA

genSimpleDecisionTree ::
     forall choice result p.
     Ord choice
  => Ord result
  => EuclideanRing p
  => Gen p
  -> Indexed.NonEmpty Map choice (NonEmpty Set result)
  -> Gen (SimpleDecisionTree p choice result)
genSimpleDecisionTree genProb m =
  unsafeFromRightBecause "Using same skeleton for each" <<<
  SDT.make <$> traverse (genDist genProb) m

genDist ::
     forall a p.
     Ord a
  => EuclideanRing p
  => Gen p
  -> NonEmpty Set a
  -> Gen (Dist p a)
genDist genProb results = do
  probs <- Gen.listOf (length results) genProb
  pure $
    Dist.make <<<
    unsafeFromJustBecause "results is `NonEmpty`" <<<
    nonEmptyMap <<< Map.fromFoldable $
    List.zip (Set.toUnfoldable $ fromNonEmpty Set.insert results) probs

b :: Int -> BigInt
b = BigInt.fromInt

genProb :: Gen Prob
-- genProb = MkProb <<< (_ % b 100) <<< b <$> Gen.chooseInt 0 100
genProb = MkProb <$> Gen.uniform

genResult :: Gen DResult
-- genResult = (\n d -> b n % b d) <$> Gen.chooseInt (-100) 100 <*> Gen.chooseInt 1 100
genResult = Gen.choose (-100.0) 100.0

genStringNumTree :: Gen (InvestigationAndDecisionTree Prob String String DResult)
genStringNumTree =
  genInvestigationAndDecisionTree
    (un AlphaNumString <$> arbitrary)
    (un AlphaNumString <$> arbitrary)
    genResult
    genProb

investigationAndDecisionTreeValue' ::
     forall finding choice.
     Ord choice
  => Ord finding
  => InvestigationAndDecisionTree Prob finding choice Number
  -> Tagged Value Number
investigationAndDecisionTreeValue' =
  investigationAndDecisionTreeValue tagged evMaxDecide' evMaxValue'

-- investigationAndDecisionTreeValueX' =
--   investigationAndDecisionTreeValueX tagged evMaxDecide' evMaxValue'

simpleDecisionTreeValue' ::
     forall choice.
     Ord choice
  => SimpleDecisionTree Prob choice Number
  -> Tagged Value Number
simpleDecisionTreeValue' =
  simpleDecisionTreeValue tagged evMaxDecide' evMaxValue'

investigationAndDecisionTreeDecide' ::
     forall finding choice.
     Ord finding
  => Ord choice
  => InvestigationAndDecisionTree Prob finding choice Number
  -> Indexed.NonEmpty Map finding choice
investigationAndDecisionTreeDecide' =
  investigationAndDecisionTreeDecide evMaxDecide'

evMaxDecide' :: forall choice. Ord choice => SimpleDecisionTree Prob choice Number -> choice
evMaxDecide' = evMaxDecide tagged probToValue

evMaxValue' :: Dist Prob (Tagged Value Number) -> Tagged Value Number
evMaxValue' = evMaxValue probToValue

evoiNonNeg :: InvestigationAndDecisionTree Prob String String DResult -> Result
evoiNonNeg tree =
  voi' >=
  (-1e-5) <?> "tree:\n" <> show tree <> "\nvoi:\n" <> show voi' <>
  "\nforgotten:\n" <>
  show tree' <>
  -- "\ninfoValueX:\n" <>
  -- show (investigationAndDecisionTreeValueX' tree) <>
  -- "\ninfoValueXY:\n" <>
  -- show (evMaxValue' $ investigationAndDecisionTreeValueX' tree) <>
  "\ninfoValue:\n" <>
  show (untagged $ investigationAndDecisionTreeValue' tree) <>
  "\nsimpleValue:\n" <>
  show (untagged $ simpleDecisionTreeValue' tree') <>
  "\ninfoDecision:\n" <>
  show (Indexed.fromNonEmpty Map.insert $ investigationAndDecisionTreeDecide' tree) <>
  "\nsimpleDecision:\n" <>
  show (evMaxDecide' tree')
  where
    voi' = voi tree
    tree' = forget tree

evoiChangeIffPositive ::
     InvestigationAndDecisionTree Prob String String Rational -> Result
evoiChangeIffPositive tree = unsafeCrashWith "evoiChangeIffPositive"

evoiPerfectBest :: InvestigationAndDecisionTree Prob String String DResult -> Result
evoiPerfectBest tree = voi (perfect $ forget tree) >=? voi tree

perfectForgetRoundTrip :: InvestigationAndDecisionTree Prob String String DResult -> Result
perfectForgetRoundTrip tree = forget tree === (forget <<< perfect <<< forget $ tree)

type DResult = Number
