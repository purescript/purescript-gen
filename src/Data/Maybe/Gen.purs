module Data.Maybe.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseFloat)

import Data.Maybe (Maybe(..))

genMaybe :: forall m a. MonadGen m => m a -> m (Maybe a)
genMaybe gen = do
  f <- chooseFloat 0.0 1.0
  if f < 0.75
    then Just <$> gen
    else pure Nothing
