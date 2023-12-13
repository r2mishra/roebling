module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified ProgressBarTest as P
import qualified DurationTest as D
import qualified WidgetsTest as W
import qualified ResultLoggerTest as R
import qualified Attacker.PacerTest as PacerTest

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
