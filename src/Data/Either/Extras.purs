module Data.Either.Extras where

import Prelude

import Data.Either (Either(Left, Right))
import Data.List (List(..), mapMaybe, (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))

left :: forall a b. Either a b -> Maybe a
left (Left a) = Just a
left (Right _) = Nothing

right :: forall a b. Either a b -> Maybe b
right (Left _) = Nothing
right (Right b) = Just b

unzipEithers :: forall a b. List (Either a b) -> Either (NonEmpty List a) (List b)
unzipEithers eithers =
  case mapMaybe left eithers of
    Nil -> Right <<< mapMaybe right $ eithers
    l : ls -> Left (l :| ls)
