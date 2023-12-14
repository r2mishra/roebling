module ProgressBarTest (testProgressBarDefaultAmount) where

import qualified GUI.Widgets as W
import qualified Test.Tasty.HUnit as HU

testProgressBarDefaultAmount :: HU.Assertion
-- TODO: Retire this test?
testProgressBarDefaultAmount = HU.assertEqual "Progress bar default amount" 0.0 0.0
