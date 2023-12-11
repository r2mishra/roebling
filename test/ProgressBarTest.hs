module ProgressBarTest (testProgressBarDefaultAmount) where

import qualified Test.Tasty.HUnit as HU
import qualified ProgressBar as P

testProgressBarDefaultAmount :: HU.Assertion
testProgressBarDefaultAmount = HU.assertEqual "Progress bar default amount" 0.0 (P._x P.initialState)