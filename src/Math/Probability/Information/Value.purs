module Math.Probability.Information.Value where

import Prelude

import Data.Functor.Tagged (Tagged)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty.Indexed (deindex, index)
import Data.NonEmpty.Indexed as Indexed
import Data.Semigroup.Foldable.Extras (maximum)
import Data.Set as Set
import Data.Tuple (Tuple(Tuple), snd, swap)
import Math.Probability (expected, focus, norm, reshape)
import Math.Probability.Dist (Dist)
import Math.Probability.Dist as Dist
import Math.Probability.Information.Value.InvestigationAndDecisionTree (InvestigationAndDecisionTree)
import Math.Probability.Information.Value.InvestigationAndDecisionTree as IDT
import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree, flatDist)
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility (second, unsafeFromJustBecause, unsafeFromRightBecause, unsafeLookupBecause)


data Value

valueOfInformation ::
     forall finding choice result prob n.
     Ord finding
  => Ord choice
  => Ord result
  => Ord n
  => Bounded prob
  => Semigroup prob
  => Semiring prob
  => Ring n
  => (result -> Tagged Value n)
  -> DecisionRule prob choice result
  -> ValuationRule prob n
  -> InvestigationAndDecisionTree prob finding choice result
  -> Tagged Value n
valueOfInformation project decide value tree =
  investigationAndDecisionTreeValue project decide value tree -
  simpleDecisionTreeValue project decide value (forget tree)

-- * Tree manipulation

forget ::
     forall finding choice result prob.
     Ord choice
  => Ord finding
  => Ord result
  => Bounded prob
  => Semigroup prob
  => Semiring prob
  => InvestigationAndDecisionTree prob finding choice result
  -> SimpleDecisionTree prob choice result
forget tree =
  unsafeFromRightBecause sdtReason <<<
  SDT.make <<<
  index choose (Map.fromFoldable <<< Set.map choose) <<< IDT.choices $
  tree
  where
    choose choice =
      Tuple choice <<<
      norm <<<
      join <<< map (unsafeFromJustBecause keyReason <<< snd) <<< IDT.choose choice $
      tree
    sdtReason = "An `IDT` shouldn't have overlapping results"
    keyReason = "The keys come from the map itself"

perfect ::
     forall choice result prob.
     Ord result
  => Ord choice
  => Bounded prob
  => EuclideanRing prob
  => Semigroup prob
  => SimpleDecisionTree prob choice result
  -> InvestigationAndDecisionTree prob (Indexed.NonEmpty Map choice result) choice result
perfect = unsafeFromRightBecause idtReason <<< IDT.make <<< perfectIDT
  where
    idtReason =
      "Each branch of the `IDT` has the same `SDT` skeleton by construction"
    perfectIDT sdt = norm do
      choicesAndResults <- flatDist sdt
      pure <<<
        Tuple choicesAndResults <<<
        perfectSDT (Indexed.fromNonEmpty Map.insert choicesAndResults) $
        sdt
    perfectSDT choicesAndResults =
      unsafeFromRightBecause sdtReason <<<
      SDT.mapWithIndex
        (\choice ->
           norm <<< reshape (focus (unsafeLookupBecause lookupReason choice choicesAndResults)))
      where
        sdtReason = "`focus` doesn't adjust the set of results"
        lookupReason = "`flatDist` produces `choice`s from the `SDT` itself"

-- * Decision rules

type DecisionRule prob choice result = SimpleDecisionTree prob choice result -> choice
type ValuationRule prob n = Dist prob (Tagged Value n) -> Tagged Value n

evMaxDecide ::
     forall choice result prob n.
     Ord choice
  => Ord n
  => Semiring n
  => Semiring prob
  => (result -> Tagged Value n)
  -> (prob -> Tagged Value n)
  -> DecisionRule prob choice result
evMaxDecide projectResult projectProb =
  snd <<<
  maximum <<<
  map swap <<<
  asList <<<
  deindex identity Map.toUnfoldable <<<
  map (evMaxValue projectProb <<< map projectResult) <<< SDT.unmake
  where
    asList :: forall a. NonEmpty List a -> NonEmpty List a
    asList = identity

evMaxValue ::
     forall prob n.
     Ord n
  => Semiring n
  => Semiring prob
  => (prob -> Tagged Value n)
  -> ValuationRule prob n
evMaxValue = expected

simpleDecisionTreeValue ::
     forall choice result prob n.
     Ord choice
  => (result -> Tagged Value n)
  -> DecisionRule prob choice result
  -> ValuationRule prob n
  -> SimpleDecisionTree prob choice result
  -> Tagged Value n
simpleDecisionTreeValue project decide value tree =
  value <<<
  map project <<<
  unsafeLookupBecause reason (decide tree) <<<
  Indexed.fromNonEmpty Map.insert <<< SDT.unmake $
  tree
  where
    reason =
      "`DecisionRule` should always produce `choice` that actually exists"

investigationAndDecisionTreeDecide ::
     forall finding choice result prob.
     Ord finding
  => Ord choice
  => Ord result
  => Ord prob
  => Semiring prob
  => DecisionRule prob choice result
  -> InvestigationAndDecisionTree prob finding choice result
  -> Indexed.NonEmpty Map finding choice
investigationAndDecisionTreeDecide rule =
  index (second rule) (map rule <<< Map.fromFoldable) <<<
  Dist.values <<< IDT.unmake

investigationAndDecisionTreeValue ::
     forall finding choice result prob n.
     Ord finding
  => Ord choice
  => Ord result
  => Ord n
  => Bounded prob
  => Semigroup prob
  => Semiring prob
  => (result -> Tagged Value n)
  -> DecisionRule prob choice result
  -> ValuationRule prob n
  -> InvestigationAndDecisionTree prob finding choice result
  -> Tagged Value n
investigationAndDecisionTreeValue project decide value =
  value <<< investigate <<< IDT.unmake
  where
    investigate idt = norm do
      Tuple _ sdt <- idt
      pure $ simpleDecisionTreeValue project decide value sdt
