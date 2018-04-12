module Math.Probability.Information.Value where

import Prelude

import Data.Functor.Tagged (Tagged)
import Unsafe.Coerce (unsafeCoerce)


data Value

valueOfInformation ::
     forall finding choice result prob n sdt idt d.
     Ord finding
  => Ord choice
  => Ord result
  => Ord n
  => Bounded prob
  => Semigroup prob
  => Semiring prob
  => Ring n
  => (result -> Tagged Value n)
  -> DecisionRule sdt prob choice result
  -> ValuationRule d prob n
  -> idt prob finding choice result
  -> Tagged Value n
valueOfInformation project decide value tree =
  unsafeCoerce unit

-- * Decision rules
type DecisionRule sdt prob choice result = sdt prob choice result -> choice
type ValuationRule d prob n = d prob (Tagged Value n) -> Tagged Value n
