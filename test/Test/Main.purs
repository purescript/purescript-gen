module Test.Main where

import Prelude

import Test.Assert (ASSERT, assert)
import Partial.Unsafe (unsafeCrashWith)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Gen (class MonadGen, frequency)
import Data.NonEmpty ((:|), NonEmpty(..))
import Data.Traversable (sequence)
import Data.Array (replicate, group', length)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)
import Math (remainder)
import Control.Monad.State (State, class MonadState, get, put, evalStateT)

type TestEffects = (console :: CONSOLE, assert :: ASSERT)
type Tests = Eff TestEffects Unit

newtype TestGen a = TestGen (State Number a)
derive newtype instance testGenFunctor :: Functor TestGen
derive newtype instance testGenApply :: Apply TestGen
derive newtype instance testGenBind :: Bind TestGen
derive newtype instance testGenApplicative :: Applicative TestGen
derive newtype instance testGenMonad :: Monad TestGen
derive newtype instance testGenMonadState :: MonadState Number TestGen

instance testGenMonadGen :: MonadGen TestGen where
  sized _ = unsafeCrashWith "sized should not be called"
  resize _ _ = unsafeCrashWith "resize should not be called"
  chooseBool = pure unit >>= \_ -> unsafeCrashWith "chooseBool should not be called"
  chooseFloat s e = do 
    c <- get
    put (c + 1.0)
    pure ((s + c) `remainder` e)
  chooseInt _ _ = unsafeCrashWith "chooseFloat should not be called"

runTestGen :: TestGen ~> State Number
runTestGen (TestGen x) = x

main :: Tests
main = do
  log "check frequency"
  let
    abcGen :: TestGen String
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
    abcArr = runTestGen abcArrGen `evalStateT` 0.0 # unwrap
    actual = group' abcArr <#> \(NonEmpty x xs) -> Tuple (length xs + 1) x
    expected = 
      [ (Tuple 10 "A")
      , (Tuple 20 "B")
      , (Tuple 30 "C")
      , (Tuple 40 "D")
      , (Tuple 50 "E")
      , (Tuple 50 "F")
      ]
  assert (expected == actual)
