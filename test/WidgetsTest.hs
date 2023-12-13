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
      HU.testCase "Get frac rank percentile" getFracRankPercentileTest,
      latencyTests
    ]

percentileTest :: HU.Assertion
percentileTest = HU.assertEqual "Percentile" 2.5 (W.percentile 67 [1.0, 2.0, 3.0])

-- | this should directly index into the given array
getIntRankPercentileTest :: HU.Assertion
getIntRankPercentileTest = HU.assertEqual "Get int rank percentile" 2.0 (W.getIntRankPercentile 1 [1.0, 2.0, 3.0])

-- | test to see if the midpoint method is used
getFracRankPercentileTest :: HU.Assertion
getFracRankPercentileTest = HU.assertEqual "Get frac rank percentile" 2.5 (W.getFracRankPercentile 1.8 [1.0, 2.0, 3.0])

latencyTests :: T.TestTree
latencyTests =
  T.testGroup
    "Latency Tests"
    [ HU.testCase "Latency statistics 1" latencyStatsTest1,
      HU.testCase "Latency statistics 2" latencyStatsTest2
    ]

latencyStatsTest1 :: HU.Assertion
latencyStatsTest1 = HU.assertEqual "latency statistics1" expected_ans (W.getLatencyStats input)
  where
    expected_ans = (280, 56.0, 50.0, 90.0, 90.0, 90.0, 100, 10) -- total, mean, p50, p90, p95, p99, max, min
    input = map fromRational [100, 10, 80, 50, 40]

latencyStatsTest2 :: HU.Assertion
latencyStatsTest2 = HU.assertEqual "latency statistics2" expected_ans (W.getLatencyStats input)
  where
    expected_ans = (240, 40, 40, 40, 40, 40, 40, 40) -- total, mean, p50, p90, p95, p99, max, min
    input = map fromRational [40, 40, 40, 40, 40, 40]
