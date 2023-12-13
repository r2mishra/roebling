module WidgetsTest (tests) where

import qualified GUI.Widgets as W
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

tests :: T.TestTree
tests =
  T.testGroup
    "Widgets Tests"
    [ HU.testCase "Percentile" percentileTest,
      HU.testCase "Get int rank percentile" getIntRankPercentileTest,
      HU.testCase "Get frac rank percentile" getFracRankPercentileTest
    ]

percentileTest :: HU.Assertion
percentileTest = HU.assertEqual "Percentile" 2.5 (W.percentile 67 [1.0, 2.0, 3.0])

getIntRankPercentileTest :: HU.Assertion
getIntRankPercentileTest = HU.assertEqual "Get int rank percentile" 2.0 (W.getIntRankPercentile 1.0 [1.0, 2.0, 3.0])

getFracRankPercentileTest :: HU.Assertion
getFracRankPercentileTest = HU.assertEqual "Get frac rank percentile" 2.0 (W.getFracRankPercentile 1.8 [1.0, 2.0, 3.0])
