module DurationTest (durationTests) where

import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Utils.Duration as D

durationTests :: TestTree
durationTests =
  testGroup
    "Duration Tests"
    [ HU.testCase "Duration add" testDurationAdd,
      HU.testCase "Duration sub" testDurationSub,
      HU.testCase "Duration mul" testDurationMul,
      HU.testCase "Duration div" testDurationDiv,
      HU.testCase "Duration getNs" testDurationNs,
      HU.testCase "Duration nanoseconds" testDurationNanoseconds,
      HU.testCase "Duration microseconds" testDurationMicroseconds,
      HU.testCase "Duration milliseconds" testDurationMilliseconds,
      HU.testCase "Duration seconds" testDurationSeconds,
      HU.testCase "Duration minutes" testDurationMinutes,
      HU.testCase "Duration hours" testDurationHours
    ]

testDurationAdd :: HU.Assertion
testDurationAdd = HU.assertEqual "Duration add" (D.mkDuration 2) (D.addDuration (D.mkDuration 1) (D.mkDuration 1))

testDurationSub :: HU.Assertion
testDurationSub = HU.assertEqual "Duration sub" (D.mkDuration 0) (D.subDuration (D.mkDuration 1) (D.mkDuration 1))

testDurationMul :: HU.Assertion
testDurationMul = HU.assertEqual "Duration mul" (D.mkDuration 2) (D.mulDuration (D.mkDuration 1) 2)

testDurationDiv :: HU.Assertion
testDurationDiv = HU.assertEqual "Duration div" (D.mkDuration 1) (D.divDuration (D.mkDuration 2) 2)

testDurationNs :: HU.Assertion
testDurationNs = HU.assertEqual "Duration getNs" 1 (D.getNs (D.mkDuration 1))

testDurationNanoseconds :: HU.Assertion
testDurationNanoseconds = HU.assertEqual "Duration nanoseconds" (D.mkDuration 1) (D.nanoseconds 1)

testDurationMicroseconds :: HU.Assertion
testDurationMicroseconds = HU.assertEqual "Duration microseconds" (D.mkDuration 1000000) (D.microseconds 1000)

testDurationMilliseconds :: HU.Assertion
testDurationMilliseconds = HU.assertEqual "Duration milliseconds" (D.mkDuration 1000000000) (D.milliseconds 1000)

testDurationSeconds :: HU.Assertion
testDurationSeconds = HU.assertEqual "Duration seconds" (D.mkDuration 1000000000) (D.seconds 1)

testDurationMinutes :: HU.Assertion
testDurationMinutes = HU.assertEqual "Duration minutes" (D.mkDuration 60000000000) (D.minutes 1)

testDurationHours :: HU.Assertion
testDurationHours = HU.assertEqual "Duration hours" (D.mkDuration 3600000000000) (D.hours 1)