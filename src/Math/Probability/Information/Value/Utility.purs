module Math.Probability.Information.Value.Utility where

import Prelude

import Data.Either (Either(Left, Right))
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.NonEmpty.Indexed as Indexed
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)


unfoldNEMap ::
     forall k v t.
     Ord k
  => Indexed.NonEmpty Map k v
  -> List (Tuple k v)
unfoldNEMap = Map.toUnfoldable <<< Indexed.fromNonEmpty Map.insert

unfoldNESet ::
     forall t a.
     Ord a
  => NonEmpty Set a
  -> List a
unfoldNESet = Set.toUnfoldable <<< fromNonEmpty Set.insert

unsafeNonEmptyBecause :: forall a. String -> List a -> NonEmpty List a
unsafeNonEmptyBecause _ (Cons x xs) = x :| xs
unsafeNonEmptyBecause s Nil = unsafeCrashWith s

nonEmptyToSet ::
     forall t a.
     Foldable t
  => Ord a
  => NonEmpty t a
  -> NonEmpty Set a
nonEmptyToSet (a :| ta) =
  unsafeFromJustBecause "Uniquifying never removes the last element" <<<
  nonEmptySet <<< Set.insert a <<< Set.fromFoldable $
  ta

unsafeMapFromFoldableBecause ::
     forall t k v.
     Foldable t
  => Ord k
  => String
  -> t (Tuple k v)
  -> Map k v
unsafeMapFromFoldableBecause s =
  Map.fromFoldableWith (\_ _ -> unsafeCrashWith s)

unsafeFromRightBecause :: forall a b. String -> Either a b -> b
unsafeFromRightBecause _ (Right x) = x
unsafeFromRightBecause s (Left _) = unsafeCrashWith s

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause s Nothing = unsafeCrashWith s

liftMapOverNonEmpty ::
     forall k v.
     Ord k
  => (Map k v -> Map k v)
  -> Indexed.NonEmpty Map k v
  -> Maybe (Indexed.NonEmpty Map k v)
liftMapOverNonEmpty f = nonEmptyMap <<< f <<< Indexed.fromNonEmpty Map.insert

nonEmptyMap ::
     forall k v.
     Ord k
  => Map k v
  -> Maybe (Indexed.NonEmpty Map k v)
nonEmptyMap m =
  (\l -> (Tuple l.key l.value) Indexed.:| (l.key `Map.delete` m)) <$> Map.findMin m

nonEmptySet :: forall a. Ord a => Set a -> Maybe (NonEmpty Set a)
nonEmptySet s = (\m -> m :| m `Set.delete` s) <$> Set.findMin s

unsafeNonEmptyMapBecause ::
     forall k v.
     Ord k
  => String
  -> Map k v
  -> Indexed.NonEmpty Map k v
unsafeNonEmptyMapBecause s m =
  (\l -> (Tuple l . key l . value) Indexed.:| (l . key `Map.delete` m)) <<<
  unsafeFromJustBecause s <<<
  Map.findMin $
  m

unsafeLookupBecause ::
     forall k v.
     Ord k
  => String
  -> k
  -> Map k v
  -> v
unsafeLookupBecause s k = unsafeFromJustBecause s <<< Map.lookup k

duplicates ::
     forall k t.
     Ord k
  => Foldable t
  => Functor t
  => t k
  -> Set k
duplicates =
  Set.fromFoldable <<<
  Map.keys <<<
  Map.filter (_ > 1) <<< Map.fromFoldableWith (+) <<< map (_ `Tuple` 1)

singletonSet :: forall a. a -> NonEmpty Set a
singletonSet a = a :| Set.empty