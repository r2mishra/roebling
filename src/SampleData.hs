module SampleData where
import Widgets
import Data.Time 
import Chart 
import qualified Data.Map as M
import Data.Set (fromList)


dummyDay :: Day
dummyDay = fromGregorian 2023 1 1  -- Year, Month, Day
dummyTime :: DiffTime
dummyTime = secondsToDiffTime (12 * 60 * 60)  -- Time in seconds (12 hours)
dummyUTCTime :: UTCTime
dummyUTCTime = UTCTime dummyDay dummyTime

-- Sample data for the chart
mySeries :: [Integer]
mySeries = [1..20]

myLatencies :: [NominalDiffTime]
myLatencies = map fromRational [0.8, 0.7, 0.98, 0.55, 0.66]

myoptions :: Options 
myoptions  = MkOptions { height = 14 }

myBytes :: BytesWidget
myBytes = MkBytesWidget {
    inMetrics = MkBytesMetrics {total = 1, mean= 1.0},
    outMetrics =  MkBytesMetrics {total = 1, mean= 1.0}
}

myErrors :: Errors
myErrors = MkErrors {errors = fromList ["None"]}

myStatusCodes :: StatusCodes
myStatusCodes = MkStatusCodes {statusCodes = M.fromList [("200", 1)]}

myOtherStats :: OtherStats
myOtherStats = MkOtherStats {
    wait = 1,
    requests = 10,
    throughput = 2.0,
    success = 0.5,
    earliest = dummyUTCTime,
    latest = dummyUTCTime,
    end = dummyUTCTime
}