{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ResultLoggerTest (resultLoggerTests) where

import qualified Attacker.ResultLogger as R
import Control.Concurrent
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception
import Control.Monad
import Data.Time
import System.Directory (removeFile)
import System.IO.Silently
import Test.Tasty
import Test.Tasty.HUnit
import qualified Utils.Models as M

testRunLogger :: Assertion
testRunLogger = do
  channel <- newChan

  let testFileName = "test_result.log"

  _ <- forkIO $ R.runLogger testFileName channel

  dummyUTCTime <- getCurrentTime

  writeChan channel (M.ResultMessage (M.AttackResult 10 200 50 (Just "No Error") 100 150 dummyUTCTime dummyUTCTime))

  threadDelay 1000000

  contents <- readFile testFileName

  let expectedLines = 1
  assertEqual "Number of lines in the log file" expectedLines (length $ lines contents)

  removeFile testFileName

resultLoggerTests :: TestTree
resultLoggerTests = testGroup "ResultLogger Tests" [testCase "runLogger" testRunLogger]
