module Data.Identity.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen)

import Data.Identity (Identity(..))

genIdentity :: forall m a. MonadGen m => m a -> m (Identity a)
genIdentity = map Identity
