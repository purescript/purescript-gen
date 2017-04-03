module Data.Tuple.Gen where

import Control.Apply (class Apply, lift2)

import Data.Tuple (Tuple(..))

genTuple :: forall m a b. Apply m => m a -> m b -> m (Tuple a b)
genTuple = lift2 Tuple
