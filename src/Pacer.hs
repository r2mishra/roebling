module Pacer
  ( PaceConfig,
    PacerResult,
    pace,
  )
where

import Data.Time

data PaceConfig = PaceConfig {rate :: Int, duration :: NominalDiffTime} deriving (Show)

data PacerResult = PacerResult {stop :: Bool, waitTime :: NominalDiffTime} deriving (Show)

pace :: UTCTime -> Int -> PaceConfig -> IO PacerResult
pace began hitCount config = do
  now <- getCurrentTime
  let elapsed = now `diffUTCTime` began
  if elapsed > duration config
    then return PacerResult {stop = True, waitTime = 0}
    else do
      let expectedHits = round (fromIntegral (rate config) * elapsed)
      if hitCount < expectedHits
        then return PacerResult {stop = False, waitTime = 0}
        else do
          let sleepTime = (fromIntegral (rate config) * fromIntegral (hitCount + 1)) - elapsed
          return PacerResult {stop = False, waitTime = sleepTime}
