module Main where

import qualified Attacker.PacerTest as PacerTest
import qualified DurationTest as D
import qualified ProgressBarTest as P
import qualified ResultLoggerTest as R
import Test.Tasty
import Test.Tasty.HUnit
import qualified WidgetsTest as W

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ testGroup
        "ProgressBar Tests"
        [ testCase "Progress bar default amount" P.testProgressBarDefaultAmount,
          D.durationTests,
          W.tests
        ],
      testGroup "Tests" [PacerTest.tests]
    ]

main :: IO ()
main = defaultMain tests
