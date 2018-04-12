module Math.Probability.Information.Value.InvestigationAndDecisionTree
  ( module Math.Probability.Information.Value.InvestigationAndDecisionTree
  , module ForReExport
  ) where

import Prelude

import Data.Bifunctor (bimap, lmap, rmap)
import Data.Either (Either(Left, Right), note)
import Data.Either.Extras (unzipEithers)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extras as Map
import Data.Map.Extras2 as Map
import Data.Maybe (Maybe, maybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.NonEmpty.Extras (hmap)
import Data.NonEmpty.Indexed (index)
import Data.NonEmpty.Indexed as Indexed
import Data.Profunctor.Strong (second)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (Tuple3, (/\))
import Math.Probability (norm)
import Math.Probability.Dist (Dist)
import Math.Probability.Dist as Dist
import Math.Probability.Information.Value.InvestigationAndDecisionTree.Internal (InvestigationAndDecisionTree(..))
import Math.Probability.Information.Value.InvestigationAndDecisionTree.Internal (InvestigationAndDecisionTree) as ForReExport
import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree)
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility (duplicates, nonEmptyMap, nonEmptySet, nonEmptyToSet, unfoldNEMap, unsafeLookupBecause, unsafeNonEmptyBecause)
import Partial.Unsafe (unsafeCrashWith)


data Failure finding choice result
  = DuplicateFindings (NonEmpty Set finding)
  | MissingChoices (Indexed.NonEmpty Map finding (NonEmpty Set choice))
  | MissingResults
      (Indexed.NonEmpty
         Map
         finding
         (Indexed.NonEmpty Map choice (NonEmpty Set result)))
  | SimpleDecisionTreeFailures (Indexed.NonEmpty Map finding (SDT.Failure choice result))
  | EmptyFindings
instance showFailure ::
     (Ord finding, Ord choice, Ord result, Show finding, Show choice, Show result)
  => Show (Failure finding choice result) where
  show (DuplicateFindings s) =
    "(DuplicateFindings " <> show (fromNonEmpty Set.insert s) <> ")"
  show (MissingChoices m) =
    "(MissingChoices " <>
    show (map (fromNonEmpty Set.insert) <<< Indexed.fromNonEmpty Map.insert $ m) <>
    ")"
  show (MissingResults m) =
    "(MissingResults " <>
    show
      (map (map (fromNonEmpty Set.insert) <<< Indexed.fromNonEmpty Map.insert) <<<
       Indexed.fromNonEmpty Map.insert $
       m) <>
    ")"
  show (SimpleDecisionTreeFailures m) =
    "(SimpleDecisionTreeFailures " <> show (Indexed.fromNonEmpty Map.insert m) <> ")"
  show EmptyFindings = "EmptyFindings"

make ::
     forall finding choice result prob.
     Ord choice
  => Ord finding
  => Ord result
  => Ord prob
  => Semiring prob
  => Dist prob (Tuple finding (SimpleDecisionTree prob choice result))
  -> Either
       (Failure finding choice result)
       (InvestigationAndDecisionTree prob finding choice result)
make dist = rmap (const $ MkInvestigationAndDecisionTree dist) $ validate dist

validate ::
     forall finding choice result prob.
     Ord choice
  => Ord finding
  => Ord result
  => Ord prob
  => Semiring prob
  => Dist prob (Tuple finding (SimpleDecisionTree prob choice result))
  -> Either (Failure finding choice result) Unit
validate dist = do
  _ <- lmap DuplicateFindings <<< validateFindings $ dist
  _ <- lmap MissingChoices <<< validateChoices $ dist
  _ <- lmap MissingResults <<< validateResults $ dist
  pure unit

validateFindings ::
     forall finding rest prob.
     Ord rest
  => Ord finding
  => Semiring prob
  => Dist prob (Tuple rest finding)
  -> Either (NonEmpty Set rest) Unit
validateFindings =
  maybe (Right unit) Left <<<
  nonEmptySet <<<
  duplicates <<<
  map fst <<<
  asList <<< Set.toUnfoldable <<< fromNonEmpty Set.insert <<< Dist.values
  where
    asList :: forall a. List a -> List a
    asList = id

validateChoices ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Semiring prob
  => Dist prob (Tuple finding (SimpleDecisionTree prob choice result))
  -> Either (Indexed.NonEmpty Map finding (NonEmpty Set choice)) Unit
validateChoices dist =
  maybe (Right unit) Left <<< nonEmptyMap <<< Map.mapMaybe missingChoices $
  findingsAndChoices
  where
    findingsAndChoices =
      Indexed.fromNonEmpty Map.insert <<<
      map (Set.fromFoldable <<< Map.keys <<< Indexed.fromNonEmpty Map.insert) <<<
      skeleton' (\_ _ -> unsafeCrashWith "`finding`s already validated") $
      dist
    missingChoices = nonEmptySet <<< Set.difference allChoices
    allChoices = Set.unions <<< Map.values $ findingsAndChoices

validateResults ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Semiring prob
  => Dist prob (Tuple finding (SimpleDecisionTree prob choice result))
  -> Either
       (Indexed.NonEmpty
          Map
          finding
          (Indexed.NonEmpty Map choice (NonEmpty Set result)))
       Unit
validateResults dist =
  maybe (Right unit) Left <<<
  nonEmptyMap <<<
  Map.mapMaybe (nonEmptyMap <<< Map.mapMaybeWithKey missingResults) $
  findingsChoicesAndResults
  where
    findingsChoicesAndResults =
      map (map (fromNonEmpty Set.insert) <<< Indexed.fromNonEmpty Map.insert) <<<
      Indexed.fromNonEmpty Map.insert <<<
      skeleton' (\_ _ -> unsafeCrashWith "`finding`s already validated") $
      dist
    allResults =
      Map.unionsWith Set.union <<< Map.values $ findingsChoicesAndResults
    missingResults choice =
      nonEmptySet <<< Set.difference (unsafeLookupBecause reason choice allResults)
      where
        reason = "`choice` and `allResults` both come from `findingsChoicesAndResults`"

unmake ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Semiring prob
  => InvestigationAndDecisionTree prob finding choice result
  -> Dist prob (Tuple finding (SimpleDecisionTree prob choice result))
unmake (MkInvestigationAndDecisionTree dist) = norm dist

choices ::
     forall finding choice result prob.
     Ord choice
  => Ord finding
  => Ord result
  => Ord prob
  => Semiring prob
  => InvestigationAndDecisionTree prob finding choice result
  -> NonEmpty Set choice
choices =
  SDT.choices <<< snd <<< fst <<< Indexed.head <<< Dist.unmake <<< unmake

type Skeleton finding choice result = Indexed.NonEmpty Map finding (SDT.Skeleton choice result)

skeleton' ::
     forall finding result choice prob.
     Ord finding
  => Ord result
  => Ord choice
  => Ord prob
  => Semiring prob
  => (SDT.Skeleton choice result -> SDT.Skeleton choice result -> SDT.Skeleton choice result)
  -> Dist prob (Tuple finding (SimpleDecisionTree prob choice result))
  -> Skeleton finding choice result
skeleton' f =
  -- TODO: Clean up once `instance Ord1 Set` lands
  Indexed.index id (Map.fromFoldableWith f) <<<
  map (second SDT.skeleton) <<<
  hmap (asList <<< Set.toUnfoldable) <<< Dist.values
  where
    asList :: forall a. List a -> List a
    asList = id

skeleton ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Semiring prob
  => InvestigationAndDecisionTree prob finding choice result
  -> Skeleton finding choice result
skeleton = skeleton' (\_ _ -> unsafeCrashWith reason) <<< unmake
  where
    reason = "`IDT` ensures findings are unique"

flatten ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Semiring prob
  => InvestigationAndDecisionTree prob finding choice result
  -> NonEmpty Set (Tuple3 (Tuple finding prob) choice (Tuple result prob))
flatten = nonEmptyToSet <<< unsafeNonEmptyBecause reason <<< combos <<< elementary
  where
    reason = "`IDT` cannot be empty"
    combos tree = do
      Tuple (Tuple finding choicesAndResults) prob <- tree
      Tuple choice results <- choicesAndResults
      result <- results
      pure $ (Tuple finding prob) /\ choice /\ result /\ unit

elementary ::
  forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Semiring prob
  => InvestigationAndDecisionTree prob finding choice result
  -> List (Tuple (Tuple finding (List (Tuple choice (List (Tuple result prob))))) prob)
elementary = unfoldNEMap <<< Dist.unmake <<< map (second SDT.elementary) <<< unmake

fromElementary ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => EuclideanRing prob
  => Ord prob
  => List (Tuple (Tuple finding (List (Tuple choice (List (Tuple result prob))))) prob)
  -> Either
       (Failure finding choice result)
       (InvestigationAndDecisionTree prob finding choice result)
fromElementary =
  make <<<
  Dist.make <=<
  note EmptyFindings <<<
  nonEmptyMap <<<
  Map.fromFoldable <=<
  lmap (SimpleDecisionTreeFailures <<< index id Map.fromFoldable) <<<
  unzipEithers <<< map sdtFromElementary
  where
    sdtFromElementary (Tuple (Tuple finding l) prob) =
      bimap (Tuple finding) (\s -> Tuple (Tuple finding s) prob) <<<
      SDT.fromElementary $
      l

choose ::
     forall finding choice result prob.
     Ord choice
  => Ord finding
  => Ord result
  => Bounded prob
  => Semigroup prob
  => Semiring prob
  => choice
  -> InvestigationAndDecisionTree prob finding choice result
  -> Dist prob (Tuple finding (Maybe (Dist prob result)))
choose choice (MkInvestigationAndDecisionTree tree) = norm do
  Tuple finding sdt <- tree
  pure $ Tuple finding (SDT.choose choice sdt)

lift ::
     forall finding1 choice1 result1 finding2 choice2 result2 prob.
     Ord finding1
  => Ord choice1
  => Ord result1
  => Ord finding2
  => Ord choice2
  => Ord result2
  => Ord prob
  => Semiring prob
  => (   Dist prob (Tuple finding1 (SimpleDecisionTree prob choice1 result1))
      -> Dist prob (Tuple finding2 (SimpleDecisionTree prob choice2 result2)))
  -> InvestigationAndDecisionTree prob finding1 choice1 result1
  -> Either
       (Failure finding2 choice2 result2)
       (InvestigationAndDecisionTree prob finding2 choice2 result2)
lift f = make <<< norm <<< f <<< unmake
