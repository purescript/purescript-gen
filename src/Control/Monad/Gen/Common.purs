module Control.Monad.Gen.Common where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Monad.Gen (class MonadGen, choose, chooseFloat)

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | Creates a generator that outputs `Either` values, choosing a value from a
-- | `Left` or the `Right` with even probability.
genEither :: forall m a b. MonadGen m => m a -> m b -> m (Either a b)
genEither genA genB = choose (Left <$> genA) (Right <$> genB)

-- | Creates a generator that outputs `Identity` values, choosing a value from
-- | another generator for the inner value.
genIdentity :: forall m a. Functor m => m a -> m (Identity a)
genIdentity = map Identity

-- | Creates a generator that outputs `Maybe` values, choosing a value from
-- | another generator for the inner value. The generator has a 75% chance of
-- | returning a `Just` over a `Nothing`.
genMaybe :: forall m a. MonadGen m => m a -> m (Maybe a)
genMaybe gen = do
  f <- chooseFloat 0.0 1.0
  if f < 0.75
    then Just <$> gen
    else pure Nothing

-- | Creates a generator that outputs `Tuple` values, choosing values from a
-- | pair of generators for each slot in the tuple.
genTuple :: forall m a b. Apply m => m a -> m b -> m (Tuple a b)
genTuple = lift2 Tuple
