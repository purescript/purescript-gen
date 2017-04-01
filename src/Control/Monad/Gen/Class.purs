module Control.Monad.Gen.Class where

import Prelude

-- | A class for random generator implementations.
-- |
-- | Instances should provide implementations for the generation functions
-- | that return choices with uniform probability.
class Monad m <= MonadGen m where

  -- | Chooses an integer in the specified (inclusive) range.
  chooseInt :: Int -> Int -> m Int

  -- | Chooses an floating point number in the specified (inclusive) range.
  chooseFloat :: Number -> Number -> m Number

  -- | Generates a random boolean value.
  genBool :: m Boolean

  -- | Modifies the size state for the current random generator.
  resize :: forall a. (Size -> Size) -> m a -> m a

  -- | Runs a generator, passing in the current size state.
  sized :: forall a. (Size -> m a) -> m a

type Size = Int
