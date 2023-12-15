{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Widgets where

import Attacker.Attacker
import Brick
import qualified Brick.BorderMap
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Core (textWidth)
import qualified Brick.AttrMap as A
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.ProgressBar as P
import Control.Concurrent (Chan, readChan)
import Data.List (sort)
import qualified Data.Map as M
import Data.Set (Set, toList)
import Data.Text (Text)
import Data.Time (NominalDiffTime, TimeLocale, UTCTime)
import Data.Tree (drawTree)
import GHC.Base (VecElem (DoubleElemRep))
import qualified Graphics.Vty
import Lens.Micro ((^.))
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import qualified Graphics.Vty as V
import Attacker.Attacker
import Network.URI (URI)
import Utils.Models (AttackResult (..), AttackResultMessage (..))
import Text.Printf (printf)

-- | Params is the set of attack parameters
data Params = MkParams
  { -- | target endpoint
    target :: URI,
    -- | request rate per second
    rate :: Int,
    -- | duration in seconds
    duration :: Int,
    -- | HTTP request type (GET, etc)
    method :: Text
  }

-- a :: Int
-- a = textWidth ("fasas" :: String)

-- TODO: Add centering to make sure latency plot occupies the full width
drawLatencyStats :: [NominalDiffTime] -> Widget ()
drawLatencyStats latencies =
  withBorderStyle unicode $
    borderWithLabel (str "Latency Stats(s)") $
      Brick.str (formatStats latencies)

formatStats :: [NominalDiffTime] -> String
formatStats latencies =
  unlines
    [ "Total: " ++ printf "%0.4f" totalL :: String,
      "Mean: " ++ printf "%0.4f" meanL,
      "P50: " ++ printf "%0.4f" p50,
      "P90: " ++ printf "%0.4f" p90,
      "P95: " ++ printf "%0.4f" p95,
      "P99: " ++ printf "%0.4f" p99,
      "Max: " ++ printf "%0.4f" maxL,
      "Min: " ++ printf "%0.4f" minL
    ]
  where
    (totalL, meanL, p50, p90, p95, p99, maxL, minL) = getLatencyStats latencies

getLatencyStats :: [NominalDiffTime] -> (Double, Double, Double, Double, Double, Double, Double, Double)
getLatencyStats latencies = (totalL, meanL, p50, p90, p95, p99, maxL, minL)
  where
    floatLats = map realToFrac latencies :: [Double]
    totalL = sum floatLats
    count = fromIntegral $ length floatLats
    meanL = totalL / count
    sortedList = sort floatLats
    p50 = percentile 50 sortedList
    p90 = percentile 90 sortedList
    p95 = percentile 95 sortedList
    p99 = percentile 99 sortedList
    maxL = sortedList !! (length sortedList - 1)
    minL = head sortedList

percentile :: Int -> [Double] -> Double
percentile p xs = go p xs
  where
    go p xs =
      if r == fromIntegral (floor r :: Int)
        then getIntRankPercentile (floor r) xs
        else getFracRankPercentile r xs
    r :: Double
    r = (fromIntegral p / 100.0) * fromIntegral (length xs - 1)

getIntRankPercentile :: Int -> [Double] -> Double
getIntRankPercentile r xs = xs !! r

getFracRankPercentile :: Double -> [Double] -> Double
getFracRankPercentile r xs = xs !! floor r + 0.5 * (xs !! (floor r + 1) - xs !! floor r)

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
  { totalB :: Integer,
    meanB :: Double
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
        "  Total: " ++ show (totalB $ inMetrics b),
        "  Mean: " ++ show (meanB $ inMetrics b),
        "Out:",
        "  Total: " ++ show (totalB $ outMetrics b),
        "  Mean: " ++ show (meanB $ outMetrics b)
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

drawProgressBar :: Float -> Widget ()
drawProgressBar p = hLimitPercent 65 ui
  where
    -- use mapAttrNames
    xBar =
      updateAttrMap
        ( A.mapAttrNames
            [ (xDoneAttr, P.progressCompleteAttr),
              (xToDoAttr, P.progressIncompleteAttr)
            ]
        )
        $ bar
        $ p
    lbl c = Just $ show $ fromEnum $ c * 100
    bar v = P.progressBar (lbl v) v
    ui = xBar

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (theBaseAttr, bg V.white),
      (xDoneAttr, V.white `on` V.white),
      (xToDoAttr, V.white `on` V.white),
      (P.progressIncompleteAttr, fg V.white)
    ]

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

drawLegend :: Widget ()
drawLegend =
  hLimit 30 $
    withBorderStyle unicode $
      borderWithLabel (str "Legend") $
        Brick.str "q: Quit"
