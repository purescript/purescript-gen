module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Frequency as Frequency

main :: Effect Unit
main = do
  log "check frequency"
  Frequency.check
