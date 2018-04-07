module Math.Probability.Information.Value (
  module Math.Probability.Information.Value,
  module VisibleInternals) where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Tagged (Tagged)
import Data.Map (Map)
import Data.Maybe (Maybe, maybe)
import Data.NonEmpty (NonEmpty)
import Data.Tuple (Tuple)
import Math.Probability (Dist)
import Math.Probability.Information.Value.Internal
  (InvestigationAndDecisionTree(..), SimpleDecisionTree)
import Math.Probability.Information.Value.Internal
  (SimpleDecisionTree, InvestigationAndDecisionTree) as VisibleInternals
import Unsafe.Coerce (unsafeCoerce)

type DecisionRule choice result = SimpleDecisionTree choice result -> choice

data Value

expectedValueOfInformation ::
  forall finding choice result.
  (result -> Tagged Value Number) ->
  DecisionRule choice result ->
  InvestigationAndDecisionTree finding choice result ->
  Tagged Value Number
expectedValueOfInformation project decide (MkInvestigationAndDecisionTree tree) = unsafeCoerce unit

evMax :: forall choice result. (result -> Tagged Value Number) -> DecisionRule choice result
evMax project choices = unsafeCoerce unit

data Failure finding choice result
  = MissingsChoices (NonEmpty (Map finding) (NonEmpty Array choice))
  | MissingResults (NonEmpty (Map finding) (NonEmpty (Map choice) (NonEmpty Array result)))

mkInvestigationAndDecisionTree ::
  forall finding choice result.
  Dist (Tuple finding (SimpleDecisionTree choice result)) ->
  Either (Failure finding choice result) (InvestigationAndDecisionTree finding choice result)
mkInvestigationAndDecisionTree dist =
  maybe (Right $ MkInvestigationAndDecisionTree dist) Left $ validate dist

validate ::
  forall finding choice result.
  Dist (Tuple finding (SimpleDecisionTree choice result)) ->
  Maybe (Failure finding choice result)
validate dist = unsafeCoerce unit

extract ::
  forall finding choice result.
  InvestigationAndDecisionTree finding choice result ->
  Dist (Tuple finding (SimpleDecisionTree choice result))
extract (MkInvestigationAndDecisionTree dist) = dist
