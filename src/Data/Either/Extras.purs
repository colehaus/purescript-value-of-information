module Data.Either.Extras where

import Prelude

import Control.Alternative (empty)
import Data.Either (Either(Left, Right), either)
import Data.List (List(..), mapMaybe, (:))
import Data.NonEmpty (NonEmpty, (:|))


unzipEithers :: forall a b. List (Either a b) -> Either (NonEmpty List a) (List b)
unzipEithers eithers =
  case mapMaybe left eithers of
    Nil -> Right <<< mapMaybe right $ eithers
    l : ls -> Left (l :| ls)
  where
    left = either pure (const empty)
    right = either (const empty) pure


