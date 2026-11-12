module Main where

import FiniteAutomataSpec qualified as FASpec
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
  count <- runTestTT FASpec.tests
  if errors count == 0 && failures count == 0
    then exitSuccess
    else exitFailure
