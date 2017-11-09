module Test.Main where

import Prelude

import Test.Frequency as Frequency
import Test.Assert (ASSERT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

type Tests = Eff (console :: CONSOLE, assert :: ASSERT) Unit

main :: Tests
main = do
  log "check frequency"
  Frequency.check
  