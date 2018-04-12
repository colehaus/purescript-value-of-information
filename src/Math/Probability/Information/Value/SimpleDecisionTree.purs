module Math.Probability.Information.Value.SimpleDecisionTree
  ( module Math.Probability.Information.Value.SimpleDecisionTree
  , module ForReExport
  ) where

import Prelude

import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), note)
import Data.Either.Extras (unzipEithers)
import Data.Foldable (fold)
import Data.FunctorWithIndex as F
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Math.Probability (marginalize, norm)
import Math.Probability.Dist (Dist)
import Math.Probability.Dist as Dist
import Math.Probability.Information.Value.SimpleDecisionTree.Internal (SimpleDecisionTree(..))
import Math.Probability.Information.Value.SimpleDecisionTree.Internal (SimpleDecisionTree) as ForReExport
import Math.Probability.Information.Value.Utility (nonEmptyMap, nonEmptySet, nonEmptyToSet, unfoldNEMap, unsafeNonEmptyBecause, unsafeNonEmptyMapBecause)


data Failure choice result
  = EmptyChoices
  | EmptyResults (NonEmpty Set choice)
  | OverlappingResults (NonEmpty Set (Tuple (Set result) (Set result)))

instance showFailure ::
    (Ord choice, Ord result, Show choice, Show result)
  => Show (Failure choice result) where
  show (OverlappingResults set) =
    "(OverlappingResults " <> show (fromNonEmpty Set.insert set) <> ")"
  show EmptyChoices = "EmptyChoices"
  show (EmptyResults choice) =
    "(EmptyResults " <> show (fromNonEmpty Set.insert choice) <> ")"

make ::
     forall choice result prob.
     Semiring prob
  => Ord choice
  => Ord result
  => Indexed.NonEmpty Map choice (Dist prob result)
  -> Either (Failure choice result) (SimpleDecisionTree prob choice result)
make map = rmap (const $ MkSimpleDecisionTree map) $ validate map

validate ::
     forall choice result prob.
     Ord choice
  => Ord result
  => Semiring prob
  => Indexed.NonEmpty Map choice (Dist prob result)
  -> Either (Failure choice result) Unit
validate =
  maybe (Right unit) Left <<<
  map OverlappingResults <<<
  nonEmptySet <<<
  Set.fromFoldable <<<
  List.mapMaybe overlap <<<
  pairs <<<
  map (Set.fromFoldable <<< Dist.values) <<<
  Map.values <<< Indexed.fromNonEmpty Map.insert
  where
    pairs xs = do
      x1 <- xs
      x2 <- xs
      pure $ Tuple x1 x2
    overlap (Tuple sl sr)
      | sl /= sr && sl `Set.intersection` sr /= Set.empty = Just (Tuple sl sr)
      | otherwise = Nothing

unmake ::
     forall choice result prob.
     SimpleDecisionTree prob choice result
  -> Indexed.NonEmpty Map choice (Dist prob result)
unmake (MkSimpleDecisionTree map) = map

type Skeleton choice result = Indexed.NonEmpty Map choice (NonEmpty Set result)

skeleton ::
     forall choice result prob.
     Ord result
  => Semiring prob
  => SimpleDecisionTree prob choice result
  -> Skeleton choice result
skeleton (MkSimpleDecisionTree tree) = Dist.values <$> tree

flatDist ::
     forall choice result prob.
     Ord choice
  => Ord result
  => Bounded prob
  => Semigroup prob
  => EuclideanRing prob
  => SimpleDecisionTree prob choice result
  -> Dist prob (Indexed.NonEmpty Map choice result)
flatDist =
  norm <<< map (unsafeNonEmptyMapBecause reasonNE <<< Map.fromFoldable <<< fold) <<<
  traverse (uncurry addChoice) <<< unfoldNEMap <<< unmake
  where
    reasonNE = "An `SDT` should have at least one `choice`"
    addChoice choice = marginalize (Set.singleton <<< Tuple choice)

elementary ::
     forall choice result prob.
     Ord choice
  => Ord result
  => Semiring prob
  => SimpleDecisionTree prob choice result
  -> List (Tuple choice (List (Tuple result prob)))
elementary = unfoldNEMap <<< map (unfoldNEMap <<< Dist.unmake) <<< unmake

fromElementary ::
     forall choice result prob.
     Ord choice
  => Ord result
  => EuclideanRing prob
  => List (Tuple choice (List (Tuple result prob)))
  -> Either (Failure choice result) (SimpleDecisionTree prob choice result)
fromElementary =
  make <=<
  note EmptyChoices <<<
  nonEmptyMap <<<
  Map.fromFoldable <=<
  lmap (EmptyResults <<< nonEmptyToSet) <<<
  unzipEithers <<< map (uncurry resultDist)
  where
    resultDist choice =
      map (Tuple choice) <<<
      note choice <<< map Dist.make <<< nonEmptyMap <<< Map.fromFoldable

choices ::
     forall choice result prob.
     Ord choice
  => SimpleDecisionTree prob choice result
  -> NonEmpty Set choice
choices =
  nonEmptyToSet <<<
  unsafeNonEmptyBecause reason <<<
  Map.keys <<< Indexed.fromNonEmpty Map.insert <<< unmake
  where
    reason = "`SDT` cannot be empty"

choose ::
     forall choice result prob.
     Ord choice
  => Ord result
  => Semiring prob
  => choice
  -> SimpleDecisionTree prob choice result
  -> Maybe (Dist prob result)
choose choice = map norm <<< Map.lookup choice <<< Indexed.fromNonEmpty Map.insert <<< unmake

mapWithIndex ::
     forall choice result1 result2 prob.
     Ord choice
  => Ord result2
  => Semiring prob
  => (choice -> Dist prob result1 -> Dist prob result2)
  -> SimpleDecisionTree prob choice result1
  -> Either (Failure choice result2) (SimpleDecisionTree prob choice result2)
mapWithIndex f = lift $ F.mapWithIndex (\c d -> norm $ f c d)

lift ::
     forall choice1 result1 choice2 result2 prob.
     Ord choice2
  => Ord result2
  => Semiring prob
  => (   Indexed.NonEmpty Map choice1 (Dist prob result1)
      -> Indexed.NonEmpty Map choice2 (Dist prob result2))
  -> SimpleDecisionTree prob choice1 result1
  -> Either (Failure choice2 result2) (SimpleDecisionTree prob choice2 result2)
lift f = make <<< f <<< unmake
