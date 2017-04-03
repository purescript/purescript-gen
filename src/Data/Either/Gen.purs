module Data.Either.Gen where

import Prelude

import Control.Alt (class Alt)

import Data.Either (Either(..), choose)

genEither :: forall m a b. Alt m => m a -> m b -> m (Either a b)
genEither = choose
