module Attacker.Pacer
  ( PacerResult (..),
    PaceConfig (..),
    pace,
  )
where

import Data.Time
import Utils.Models (PaceConfig (..), PacerResult (..))
import System.Posix.Internals (puts)
import Brick (put)

pace :: UTCTime -> Int -> PaceConfig -> IO PacerResult
pace began hitCount config = do
  now <- getCurrentTime
  let elapsed = now `diffUTCTime` began
  putStrLn $ "Elapsed time: " ++ (show elapsed)
  if elapsed >= duration config
    then return PacerResult {stop = True, waitTime = 0}
    else do
      let expectedHits = fromIntegral (rate config) * elapsed
      putStrLn $ "Expected hits: " ++ (show expectedHits) ++ "\n"
      putStrLn $ "Actual hits: " ++ (show hitCount) ++ "\n"
      if fromIntegral hitCount < expectedHits
        then return PacerResult {stop = False, waitTime = 0}
        else do
          let sleepTime = fromIntegral (hitCount) - expectedHits
          putStrLn $ "Sleeping for " ++ (show sleepTime) ++ " seconds"
          return PacerResult {stop = False, waitTime = sleepTime}
