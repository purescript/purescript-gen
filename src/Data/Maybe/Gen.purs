module Data.Maybe.Gen where

import Control.Monad.Gen (class MonadGen, alt)

import Data.Maybe (Maybe)

genMaybe :: forall m a. MonadGen m => m a -> m (Maybe a)
genMaybe = alt
