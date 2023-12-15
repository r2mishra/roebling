module GUI.SampleData where

import qualified Data.Map as M
import Data.Set (fromList)
import Data.Time
import GUI.Chart
import GUI.Widgets
import System.Random

dummyDay :: Day
dummyDay = fromGregorian 2023 1 1 -- Year, Month, Day

dummyTime :: DiffTime
dummyTime = secondsToDiffTime (12 * 60 * 60) -- Time in seconds (12 hours)

dummyUTCTime :: UTCTime
dummyUTCTime = UTCTime dummyDay dummyTime

-- Sample data for the chart
mySeries :: [Integer]
mySeries = [1 .. 20]

repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n

myLatencies :: [NominalDiffTime]
myLatencies = map fromRational [0.1]

myoptions :: Options
myoptions = MkOptions {height = 14}

myBytes :: BytesWidget
myBytes =
  MkBytesWidget
    { inMetrics = MkBytesMetrics {totalB = 1, meanB = 1.0},
      outMetrics = MkBytesMetrics {totalB = 1, meanB = 1.0}
    }

myErrors :: Errors
myErrors = MkErrors {errors = fromList ["."]}

myStatusCodes :: StatusCodes
myStatusCodes = MkStatusCodes {statusCodes = M.fromList [("200", 0)]}

myOtherStats :: OtherStats
myOtherStats =
  MkOtherStats
    { wait = 1,
      requests = 10,
      throughput = 2.0,
      success = 0.5,
      earliest = dummyUTCTime,
      latest = dummyUTCTime,
      end = dummyUTCTime
    }

generateRandomDouble :: IO Double
generateRandomDouble = do
  g <- newStdGen -- Create a new random number generator
  let (value, _) = randomR (0.0, 1.0) g -- Generate a random Double between 0.0 and 1.0
  return value
