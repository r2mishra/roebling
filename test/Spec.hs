module Main where

import Test.Tasty
import qualified Attacker.PacerTest as PacerTest

tests:: TestTree
tests = testGroup "Tests" [PacerTest.tests]

main :: IO ()
main = defaultMain tests
