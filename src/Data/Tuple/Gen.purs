module Data.Tuple.Gen where

import Control.Apply (lift2)
import Control.Monad.Gen (class MonadGen)

import Data.Tuple (Tuple(..))

genTuple :: forall m a b. MonadGen m => m a -> m b -> m (Tuple a b)
genTuple = lift2 Tuple
