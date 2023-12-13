module ProgressBarTest (testProgressBarDefaultAmount) where

import qualified GUI.ProgressBar as P
import qualified Test.Tasty.HUnit as HU

testProgressBarDefaultAmount :: HU.Assertion
testProgressBarDefaultAmount = HU.assertEqual "Progress bar default amount" 0.0 (P._x P.initialPBState)
