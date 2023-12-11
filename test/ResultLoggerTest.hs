{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ResultLoggerTest (resultLoggerTests) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently
import Control.Exception


import ResultLogger
import Attacker


mockChannel :: IO (Chan AttackResultMessage)
mockChannel = newChan


-- Test case for 'runLogger' function
testRunLogger :: Assertion
testRunLogger = do
    chan <- mockChannel
    outputVar <- newEmptyMVar
    let expectedOutput = unlines
          [ "Logger ==> Hit: 0, Code: 200, Latency: 1.0s",
            "Logger ==> Hit: 1, Code: 200, Latency: 1.0s",
            "Logger ==> Hit: 2, Code: 200, Latency: 1.0s",
            "Logger ==> Hit: 3, Code: 200, Latency: 1.0s",
            "Logger ==> Hit: 4, Code: 200, Latency: 1.0s",
            "Logger ==> Will stop at Hit: 4"
          ]

    (outputVar, _) <- capture( withAsync (runLogger chan) $ \loggerAsync -> do    
      forM_ [0 .. 4] $ \i -> writeChan chan (ResultMessage (AttackResult i 200 1.0))
      writeChan chan (StopMessage 5)    
      threadDelay 1000000 
      catch (cancel loggerAsync) handler
      )
    -- capturedOutput <- takeMVar outputVar
    assertEqual "Output matches expected" expectedOutput  outputVar


resultLoggerTests :: TestTree
resultLoggerTests = testGroup "ResultLogger Tests" [testCase "runLogger" testRunLogger]

handler :: SomeException -> IO ()
handler _ = return ()