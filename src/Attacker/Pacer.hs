module Attacker.Pacer
  ( PacerResult (..),
    PaceConfig (..),
    pace,
  )
where

import Data.Time
import Utils.Models (PaceConfig (..), PacerResult (..))

pace :: UTCTime -> Int -> PaceConfig -> IO PacerResult
pace began hitCount config = do
  now <- getCurrentTime
  let elapsed = now `diffUTCTime` began
  -- print $ "Elapsed: " ++ show elapsed
  if elapsed > duration config
    then return PacerResult {stop = True, waitTime = 0}
    else do
      let expectedHits = fromIntegral (rate config) * elapsed
      if fromIntegral hitCount < expectedHits
        then return PacerResult {stop = False, waitTime = 0}
        else do
          let sleepTime = fromIntegral (hitCount + 1) - expectedHits
          return PacerResult {stop = False, waitTime = sleepTime}
