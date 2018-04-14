module Test.Gen where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (either)
import Data.Foldable (length)
import Data.Foldable as Foldable
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (un)
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.NonEmpty.Extras (hmap)
import Data.NonEmpty.Indexed (deindex, index)
import Data.NonEmpty.Indexed as Indexed
import Data.Ratio (Ratio, (%))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (range)
import Math.Probability.Dist as Dist
import Math.Probability.Dist.Internal (Dist)
import Math.Probability.Prob.BigInt (Prob(..))
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Data.AlphaNumString (AlphaNumString(..))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

import Math.Probability.Information.Value.InvestigationAndDecisionTree
  ( InvestigationAndDecisionTree
  )
import Math.Probability.Information.Value.InvestigationAndDecisionTree as IDT
import Math.Probability.Information.Value.SimpleDecisionTree
  ( SimpleDecisionTree
  )
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility
  ( nonEmptyMap
  , nonEmptySet
  , nonEmptyToSet
  , unsafeFromJustBecause
  , unsafeFromRightBecause
  )

b :: Int -> BigInt
b = BigInt.fromInt

genResult :: Gen Outcome
genResult = (\n d -> b n % b d) <$> Gen.chooseInt (-100) 100 <*> Gen.chooseInt 1 100

genProb :: Gen Prob
genProb = MkProb <<< (_ % b 100) <<< b <$> Gen.chooseInt 0 100

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
  => Bounded prob
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

genSimpleDecisionTree ::
     forall choice result p.
     Ord choice
  => Ord result
  => Bounded p
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
  => Bounded p
  => Eq p
  => EuclideanRing p
  => Gen p
  -> NonEmpty Set a
  -> Gen (Dist p a)
genDist genProb results = do
  probs <- Gen.suchThat (Gen.listOf (length results) genProb) ((_ /= bottom) <<< Foldable.sum)
  pure $
    Dist.make <<<
    unsafeFromJustBecause "results is `NonEmpty`" <<<
    nonEmptyMap <<< Map.fromFoldable $
    List.zip (Set.toUnfoldable $ fromNonEmpty Set.insert results) probs

genStringNumTree :: Gen (InvestigationAndDecisionTree Prob String String Outcome)
genStringNumTree =
  genInvestigationAndDecisionTree
    (un AlphaNumString <$> arbitrary)
    (un AlphaNumString <$> arbitrary)
    genResult
    genProb

type Outcome = Ratio BigInt

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

nonOverlappingSets ::
     forall a.
     Ord a
  => Int
  -> Int
  -> Gen a
  -> Gen (Set (Set a))
nonOverlappingSets total numBuckets genA = bucket numBuckets =<< sizedSetOf total genA
