module ProgressBarTest (testProgressBarDefaultAmount) where

import qualified GUI.Widgets as W
import qualified Test.Tasty.HUnit as HU

testProgressBarDefaultAmount :: HU.Assertion
testProgressBarDefaultAmount = HU.assertEqual "Progress bar default amount" 0.0 (W._x W.initialPBState)
