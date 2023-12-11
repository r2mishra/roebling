module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified ProgressBarTest as P
import qualified DurationTest as D
import qualified WidgetsTest as W
import qualified ResultLoggerTest as R

tests :: TestTree
tests =

  testGroup
    "ProgressBar Tests"
    [ testCase "Progress bar default amount" P.testProgressBarDefaultAmount,
      D.durationTests,
      W.tests
      -- R.resultLoggerTests
    ]
    

main :: IO ()
main = defaultMain tests