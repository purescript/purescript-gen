module Data.Identity.Gen where

import Prelude

import Data.Identity (Identity(..))

genIdentity :: forall m a. Functor m => m a -> m (Identity a)
genIdentity = map Identity
