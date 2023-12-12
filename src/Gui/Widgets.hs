{-# LANGUAGE InstanceSigs #-}

module GUI.Widgets where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import qualified Brick.Widgets.Border.Style as BS
import Control.Concurrent (Chan, readChan)
import Data.List (sort)
import qualified Data.Map as M
import Data.Set (Set, toList)
import Data.Text (Text)
import Data.Time (NominalDiffTime, TimeLocale, UTCTime)
import Data.Tree (drawTree)
import GHC.Base (VecElem (DoubleElemRep))
import Utils.Models (AttackResult (..), AttackResultMessage (..))

-- | Params is the set of attack parameters
data Params = MkParams
  { target :: Text, -- target endpoint
    rate :: Int, -- request rate per second
    duration :: Int, -- duration in seconds
    method :: Text -- HTTP request type (GET, etc)
  }

-- TODO: Add centering to make sure latency plot occupies the full width
drawLatencyStats :: [NominalDiffTime] -> Widget ()
drawLatencyStats latencies =
  withBorderStyle unicode $
    borderWithLabel (str "Latencies") $
      Brick.str (formatStats latencies)

formatStats :: [NominalDiffTime] -> String
formatStats latencies =
  unlines
    [ "Total: " ++ show total,
      "Mean: " ++ show mean,
      "P50: " ++ show p50,
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
    go p xs =
      if r == fromIntegral (floor r :: Int)
        then getIntRankPercentile r xs
        else getFracRankPercentile r xs
    r :: Double
    r = ((fromIntegral p) / 100.0) * fromIntegral (length xs - 1)

getIntRankPercentile :: Double -> [Double] -> Double
getIntRankPercentile r xs = xs !! floor r

getFracRankPercentile :: Double -> [Double] -> Double
getFracRankPercentile r xs = xs !! floor r + 0.5 * (xs !! (floor r + 1) - xs !! round r)

instance Show Params where
  show :: Params -> String
  show paramstext =
    unlines
      [ "Target: " ++ show (target paramstext),
        "Rate: " ++ show (rate paramstext),
        "Duration: " ++ show (duration paramstext),
        "Method: " ++ show (method paramstext)
      ]

drawParams :: Params -> Widget ()
drawParams p =
  withBorderStyle unicode $
    borderWithLabel (str "Params") $
      Brick.str (show p)

data BytesMetrics = MkBytesMetrics
  { total :: Int,
    mean :: Double
  }

data BytesWidget = MkBytesWidget
  { inMetrics :: BytesMetrics,
    outMetrics :: BytesMetrics
  }

instance Show BytesWidget where
  show :: BytesWidget -> String
  show b =
    unlines
      [ "In:",
        "  Total: " ++ show (total $ inMetrics b),
        "  Mean: " ++ show (mean $ inMetrics b),
        "Out:",
        "  Total: " ++ show (total $ outMetrics b),
        "  Mean: " ++ show (mean $ outMetrics b)
      ]

drawBytes :: BytesWidget -> Widget ()
drawBytes b =
  withBorderStyle unicode $
    borderWithLabel (str "Bytes") $
      Brick.str (show b)

newtype StatusCodes = MkStatusCodes
  { statusCodes :: M.Map String Int
  }

instance Show StatusCodes where
  show :: StatusCodes -> String
  show s = unlines $ map (\(k, v) -> show k ++ ": " ++ show v) (M.toList codes)
    where
      codes = statusCodes s

-- TODO: Resize to make the full widget Label appear
drawStatusCodes :: StatusCodes -> Widget ()
drawStatusCodes s =
  hLimit width $
    withBorderStyle unicode $
      borderWithLabel (str "StatusCodes") $
        Brick.str (show s)
  where
    width = 15

-- | Other important statistics for the attack such as throughput, success rate, etc
data OtherStats = MkOtherStats
  { -- | Wait is the extra time spent waiting for requests from target
    wait :: NominalDiffTime,
    -- | Requests is the number of requests executed
    requests :: Int,
    -- | Throughput is the rate of successful requests per second
    throughput :: Double,
    -- | Success is the percentage of non-error responses
    success :: Double,
    -- | Earliest is the earliest timestamp of a request
    earliest :: UTCTime,
    -- | Latest is the latest timestamp of a request
    latest :: UTCTime,
    -- | End is the latest timestamp of a response i.e `latest` + request latency
    end :: UTCTime
  }

instance Show OtherStats where
  show :: OtherStats -> String
  show os =
    unlines
      [ "Wait: " ++ show (wait os),
        "Requests: " ++ show (requests os),
        "Throughput: " ++ show (throughput os),
        "Success: " ++ show (success os),
        "Earliest: " ++ show (earliest os),
        "Latest: " ++ show (latest os),
        "End: " ++ show (end os)
      ]

drawOtherStats :: OtherStats -> Widget ()
drawOtherStats os =
  withBorderStyle unicode $
    borderWithLabel (str "OtherStats") $
      Brick.str (show os)

-- | Errors is the set of unique errors returned by the target server
newtype Errors = MkErrors
  { errors :: Set String
  }

instance Show Errors where
  show :: Errors -> String
  show e = unlines $ map show (toList $ errors e)

drawErrors :: Errors -> Widget ()
drawErrors e =
  withBorderStyle unicode $
    borderWithLabel (str "Errors") $
      Brick.str (show e)

-- updatePlot :: Chan AttackResultMessage -> IO ()
-- updatePlot channel = do
--   loop Nothing
--   where
--     loop msg = do
--       res <- readChan channel
--       case res of
--         StopMessage hitCount -> do
--           -- print $ "Logger ==> Will stop at Hit: " ++ show hitCount
--           loop $ Just (hitCount - 1)
--         ResultMessage (AttackResult hitCount code latency) -> do
--           if msg /= Just (hitCount + 1)
--             then do
--               -- print $ "Logger ==> Hit: " ++ show hitCount ++ ", Code: " ++ show code ++ ", Latency: " ++ show latency

--               loop msg
--             else do
--               return ()
