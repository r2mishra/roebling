{-# LANGUAGE InstanceSigs #-}
module Widgets where
import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Border.Style (unicode)
import Data.Text (Text)
import Data.List (sort)
import Data.Time (NominalDiffTime)
import Data.Tree (drawTree)
import GHC.Base (VecElem(DoubleElemRep))

data ParamsText = MakeParams {
    target :: Text, -- target endpoint
    rate :: Int, -- request rate per second
    duration :: Int, -- duration in seconds
    method :: Text -- HTTP request type (GET, etc)
}


drawLatencyStats :: [NominalDiffTime] -> Widget ()
drawLatencyStats latencies = withBorderStyle unicode
    $ borderWithLabel (str "Latencies")
    $  Brick.str (formatStats latencies)

formatStats :: [NominalDiffTime] -> String
formatStats latencies  = unlines [
    "Total: " ++ show total,
    "Mean: " ++ show mean,
    "P50: "++ show p50,
    "P90: " ++ show p90,
    "P95: " ++ show p95,
    "P99: " ++ show p99,
    "Max: " ++ show max,
    "Min: " ++ show min
    ]
    where
        floatLats = map realToFrac latencies :: [Double]
        total = sum floatLats
        count = fromIntegral $ length floatLats
        mean = total / count
        sortedList = sort floatLats
        p50 = percentile 50 sortedList
        p90 = percentile 90 sortedList
        p95 = percentile 95 sortedList
        p99 = percentile 99 sortedList
        max = sortedList !! (length sortedList - 1)
        min = head sortedList


percentile :: Int -> [Double] -> Double
percentile p xs = go p xs
    where
        go p xs = if r == fromIntegral (floor r :: Int)
            then getIntRankPercentile r xs
            else getFracRankPercentile r xs
        r :: Double
        r = ((fromIntegral p) / 100.0) * fromIntegral (length xs - 1)


getIntRankPercentile :: Double -> [Double] -> Double
getIntRankPercentile r xs = xs !! floor r

getFracRankPercentile :: Double -> [Double] -> Double
getFracRankPercentile r xs = xs !! floor r + 0.5*(xs !! (floor r + 1) - xs !! round r)

instance Show ParamsText where
    show :: ParamsText -> String
    show paramstext = unlines ["Target: " ++ show (target paramstext),
        "Rate: " ++ show (rate paramstext),
        "Duration: " ++ show (duration paramstext),
        "Method: " ++ show (method paramstext)
        ]

drawParams :: ParamsText -> Widget ()
drawParams p = withBorderStyle unicode
    $ borderWithLabel (str "ParamsText")
    $  Brick.str (show p)