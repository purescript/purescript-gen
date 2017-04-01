module Data.Either.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, merge)

import Data.Either (Either(..))

genEither :: forall m a b. MonadGen m => m a -> m b -> m (Either a b)
genEither genA genB = merge (Left <$> genA) (Right <$> genB)
