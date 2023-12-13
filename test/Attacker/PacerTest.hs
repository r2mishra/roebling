module Attacker.PacerTest where

import Attacker.Pacer
import Data.Time.Clock
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Pace should stop when duration is exceeded" testPaceStop,
      testCase "Pace should not stop when hitCount is less than expectedHits" testPaceContinue,
      testCase "Pace should calculate correct waitTime when hitCount is not less than expectedHits" testPaceWaitTime
    ]

testPaceStop :: Assertion
testPaceStop = do
  now <- getCurrentTime
  let config = PaceConfig {rate = 1, duration = 0}
  result <- pace now 1 config
  stop result @?= True

testPaceContinue :: Assertion
testPaceContinue = do
  now <- getCurrentTime
  let config = PaceConfig {rate = 2, duration = 2}
  result <- pace now 1 config
  stop result @?= False

testPaceWaitTime :: Assertion
testPaceWaitTime = do
  now <- getCurrentTime
  let config = PaceConfig {rate = 2, duration = 2}
  result <- pace now 3 config
  assertBool "Wait time should be greater than 0" (waitTime result > 0)
