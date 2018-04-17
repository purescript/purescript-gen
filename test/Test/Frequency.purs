module Test.Frequency where

import Prelude

import Control.Monad.Gen (class MonadGen, frequency)
import Control.Monad.State (State, class MonadState, get, put, evalStateT)
import Data.Array (replicate, group', length)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|), NonEmpty(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Math (remainder)
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (assert)

newtype TestGenFrequency a = TestGenFrequency (State Number a)
derive newtype instance testGenFunctor :: Functor TestGenFrequency
derive newtype instance testGenApply :: Apply TestGenFrequency
derive newtype instance testGenBind :: Bind TestGenFrequency
derive newtype instance testGenApplicative :: Applicative TestGenFrequency
derive newtype instance testGenMonad :: Monad TestGenFrequency
derive newtype instance testGenMonadState :: MonadState Number TestGenFrequency

instance testGenMonadGen :: MonadGen TestGenFrequency where
  sized _ = unsafeCrashWith "sized should not be called"
  resize _ _ = unsafeCrashWith "resize should not be called"
  chooseBool = pure unit >>= \_ -> unsafeCrashWith "chooseBool should not be called"
  chooseFloat s e = do
    c <- get
    put (c + 1.0)
    pure ((s + c) `remainder` e)
  chooseInt _ _ = unsafeCrashWith "chooseFloat should not be called"

runTestGenFrequency :: TestGenFrequency ~> State Number
runTestGenFrequency (TestGenFrequency x) = x

check :: Effect Unit
check =
  let
    abcGen :: TestGenFrequency String
    abcGen =
      frequency $
        ( Tuple 10.0 $ pure "A" ) :|
        [ Tuple 20.0 $ pure "B"
        , Tuple 0.0 $ pure "Z"
        , Tuple 30.0 $ pure "C"
        , Tuple 40.0 $ pure "D"
        , Tuple 50.0 $ pure "E"
        , Tuple 50.0 $ pure "F"
        ]
    abcArrGen = sequence $ replicate 200 abcGen
    abcArr = runTestGenFrequency abcArrGen `evalStateT` 0.0 # unwrap
    actual = group' abcArr <#> \(NonEmpty x xs) -> Tuple (length xs + 1) x
    expected =
      [ (Tuple 10 "A")
      , (Tuple 20 "B")
      , (Tuple 30 "C")
      , (Tuple 40 "D")
      , (Tuple 50 "E")
      , (Tuple 50 "F")
      ]
  in
    assert (expected == actual)
