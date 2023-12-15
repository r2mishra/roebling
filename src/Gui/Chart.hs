--- Reference for ASCII line chart: https://github.com/madnight/asciichart
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- needed for makelenses
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GUI.Chart
  ( -- * Plot
    plotWith,
    plotWith',
    Options (..),
    getPlotLines,
    height,
    AppState (..),
    plotApp,
    ui,
  )
where

import Brick.AttrMap
import qualified Brick.BorderMap
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import qualified Brick.Widgets.ProgressBar as P
import Control.Concurrent.STM (stateTVar)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST (ST, runST)
import Data.Array.ST.Safe (STArray, getElems, newArray, writeArray)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, foldl', unfoldr)
import Data.Map (alter, findWithDefault, insert)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import Data.Time (NominalDiffTime, TimeLocale (wDays))
import Data.Time.Clock (diffUTCTime)
import Debug.Trace
import GUI.Widgets (BytesWidget, OtherStats (..))
import qualified GUI.Widgets as W
import Graphics.Vty (horizCat)
import qualified Graphics.Vty
import qualified Graphics.Vty as V
import Lens.Micro (to, (&), (.~), (^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Utils.Models

appendDebugLog :: String -> IO ()
appendDebugLog msg = appendFile "debug.log" (msg ++ "\n")

data Options = MkOptions
  { -- | Allows to set the height of the chart.
    height :: Int
  }

-- Helper functions from asciichart: https://github.com/madnight/asciichart
newArray2D ::
  Integer ->
  Integer ->
  ST s (STArray s (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0, 0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

pad :: (Real a) => [a] -> Int
pad series =
  let floats = realToFrac <$> series
      toStr :: [Float] -> [String]
      toStr = fmap (printf "%0.2f")
   in maximum $ length <$> toStr floats

-- plotting function from asciichart. Modified to work with Doubles.
plotWith' :: Options -> [Double] -> [String]
plotWith' _ [] = []
plotWith' opts series =
  -- variables and functions
  let min' = minimum series
      max' = maximum series
      range = abs $ max' - min'
      offset = 3
      ratio =
        if range == 0
          then 1
          else fromIntegral (height opts) / realToFrac range :: Float
      min2 = realToFrac min' * ratio
      max2 = realToFrac max' * ratio
      rows = round $ abs $ max2 - min2
      width = toInteger $ length series + 3
   in runST $ do
        -- array creation
        arr <- newArray2D rows width
        let result x y = writeArray arr (head x, head y)

        -- axis and labels
        forM_ [min2 .. max2] $ \y -> do
          let label =
                if rows == 0
                  then y
                  else
                    realToFrac max'
                      - (y - min2)
                        * realToFrac range
                        / fromIntegral rows
          result [round $ y - min2] [max 0 $ offset - 5] $
            printf ("%" ++ show (pad series) ++ ".2f") label
          result [round $ y - min2] [offset - 1] . bool "┤" "┼" $ y == 0

        -- initial value
        let first = realToFrac (head series) * ratio - min2
        result [round $ fromInteger rows - first] [offset - 1] "┼"

        -- plot the line
        forM_ [0 .. length series - 2] $ \x -> do
          let offset' = toInteger x + offset
          let y' i = round (realToFrac (series !! i) * ratio) - round min2
          let (y0, y1) = (y' x, y' $ x + 1)
          if y0 == y1
            then result [rows - y0] [offset'] "─"
            else do
              result [rows - y1] [offset'] . bool "╭" "╰" $ y0 > y1
              result [rows - y0] [offset'] . bool "╯" "╮" $ y0 > y1

              forM_ [min y0 y1 + 1 .. max y0 y1 - 1] $ \y ->
                result [rows - y] [offset'] "│"

        getElems arr

-- -- | Takes a List of Integers and prints out a
-- --   corresponding chart with custom options for height.
--   Example: @'plotWith' options { 'height' = 20 }@
plotWith :: Options -> [Double] -> IO ()
plotWith options' series =
  forM_ result $
    putStrLn . dropWhileEnd isSpace . concat
  where
    result = splitEvery (length series + 4) $ plotWith' options' series

------ Brick UI logic ----

-- | Internal plot helper to get the ascii line chart as a list of strings
getPlotLines :: Options -> [Double] -> [String]
getPlotLines _ [] = []
getPlotLines options' series = map (dropWhileEnd isSpace . concat) result
  where
    result = splitEvery (length series + 4) $ plotWith' options' series

type Name = ()

-- | The UI's app state. Currently stores latencies, current progress, cmd flags, etc
data AppState = AppState
  { _latencies :: [NominalDiffTime],
    _numDone :: Int, -- current progress
    _hitCount :: Int, -- total number needed
    _bytesMetrics :: BytesWidget,
    _plotOptions :: Options,
    _params :: W.Params,
    _statusCodes :: W.StatusCodes,
    _reqErrors :: W.Errors,
    _otherstats :: W.OtherStats,
    _pbState :: Float,
    _numSuccess :: Int -- num of successful requests (200)
  }

makeLenses ''AppState -- provides a convenient way to access state vars while handling events

-- | The main Brick application
plotApp :: M.App AppState (Either AttackResultMessage Float) Name
plotApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

-- | The attribute map. Stores custom attributes for the progress bar.
theMap :: Brick.AttrMap.AttrMap
theMap =
  Brick.AttrMap.attrMap
    V.defAttr
    [ (theBaseAttr, bg V.brightBlack),
      (xDoneAttr, V.black `on` V.green),
      (xToDoAttr, V.white `on` V.black),
      (P.progressIncompleteAttr, fg V.black)
    ]

theBaseAttr :: Brick.AttrMap.AttrName
theBaseAttr = Brick.AttrMap.attrName "theBase"

xDoneAttr, xToDoAttr :: Brick.AttrMap.AttrName
xDoneAttr = theBaseAttr <> Brick.AttrMap.attrName "X:done"
xToDoAttr = theBaseAttr <> Brick.AttrMap.attrName "X:remaining"

-- | The plotting Widget. Automatically resizes to fit terminal width
myFillPlotWidget :: Options -> [Double] -> T.Widget n
myFillPlotWidget myoptions mylatencies =
  joinBorders $
    withBorderStyle
      unicode
      ( borderWithLabel
          (str "Latencies(s)")
          internalWidget
      )
  where
    internalWidget = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let a = ctx ^. T.attrL
      -- c <- T.getContext
      let fullWidth = ctx ^. T.availWidthL
      let curWidth = round (0.6 * fromIntegral fullWidth) -- more conservative to see updates quickly
      let cur_strings = getPlotLines myoptions mylatencies
      let cur_string_width = if not (null cur_strings) then textWidth (head cur_strings) else 0
      let newLatencies =
            if cur_string_width > curWidth
              then resizeStringList mylatencies cur_string_width curWidth
              else mylatencies
      let newStrings = getPlotLines myoptions newLatencies
      let bottomString = concat $ replicate fullWidth "-"
      let plotLines = map TL.pack (newStrings ++ [bottomString])
      let image = V.vertCat $ map (V.text V.defAttr) plotLines
      return $
        T.Result
          image
          []
          []
          []
          Brick.BorderMap.empty

keepLabelAndLastN' :: Int -> Int -> [a] -> [a]
keepLabelAndLastN' skipNum n xs = take skipNum xs ++ lastN' n (lastN' (length xs - skipNum) xs)

resizeStringList :: [Double] -> Int -> Int -> [Double]
resizeStringList mylatencies cur_string_width curWidth = lastN' subN mylatencies
  where
    subN = round (fromIntegral (length mylatencies) * (fromIntegral curWidth / fromIntegral cur_string_width))

lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

-- | Final combined UI with all the Widgets
drawUI :: AppState -> [T.Widget ()]
drawUI state = [go]
  where
    go = ui myparams myoptions mylatencies mybytes mystatuscodes myerrors myotherstats myprogressbar
    myparams = _params state
    myoptions = _plotOptions state
    mylatencies = _latencies state
    mybytes = _bytesMetrics state
    mystatuscodes = _statusCodes state
    myerrors = _reqErrors state
    myotherstats = _otherstats state
    myprogressbar = _pbState state

-- | The UI widget that includes the ASCII chart
ui :: W.Params -> Options -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes -> W.Errors -> W.OtherStats -> Float -> T.Widget ()
ui myparams myoptions mylatencies bytes statuscodes errors myotherstats myprogressbarstate =
  vBox
    [ myFillPlotWidget myoptions (map realToFrac mylatencies :: [Double]),
      fillWidgetsEvenly myparams mylatencies bytes statuscodes errors myotherstats,
      hBox
        [ W.drawProgressBar myprogressbarstate,
          W.drawLegend
        ]
    ]

fillWidgetsEvenly :: W.Params -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes -> W.Errors -> W.OtherStats -> T.Widget ()
fillWidgetsEvenly myparams mylatencies bytes statuscodes errors myotherstats =
  T.Widget T.Greedy T.Greedy $ do
    -- Compute translation offset so that loc is in the middle of the
    -- rendering area
    c <- T.getContext
    let fullWidth = c ^. T.availWidthL
        fullHeight = c ^. T.availHeightL
    let indWidth = fullWidth
    let getrightPaddingAmt result maxWidth = max 0 $ maxWidth - V.imageWidth (result ^. T.imageL)
    let getBottomPaddingAmt result maxHeight = max 0 $ maxHeight - V.imageHeight (result ^. T.imageL)
    let getRightPadding result maxWidth = V.charFill (c ^. T.attrL) ' ' (getrightPaddingAmt result maxWidth) (V.imageHeight $ result ^. T.imageL)
    let getPaddedImg result maxWidth = horizCat [result ^. T.imageL, getRightPadding result maxWidth]
    curResult <-
      T.render
        ( hBox
            [ W.drawBorder "Params" $ W.drawParams myparams,
              W.drawBorder "Params" $ W.drawLatencyStats mylatencies,
              W.drawBorder "Params" $ W.drawBytes bytes,
              vBox
                [ W.drawBorder "Params" $ W.drawStatusCodes statuscodes,
                  W.drawBorder "Params" $ W.drawErrors errors
                ],
              W.drawBorder "Params" $ W.drawOtherStats myotherstats
            ]
        )
    let curHeight = V.imageHeight (curResult ^. T.imageL)
    let equalPad = getrightPaddingAmt curResult indWidth `div` 5
    latencyResult <- T.render $ W.drawLatencyStats mylatencies
    paramResult <- T.render $ W.drawParams myparams
    bytesResult <- T.render $ W.drawBytes bytes
    errorAndStatResult <-
      T.render $
        vBox
          [ W.drawStatusCodes statuscodes,
            W.drawBorder "Errors" $ W.drawErrors errors
          ]
    errorResult <- T.render $ W.drawErrors errors
    statResult <- T.render $ W.drawStatusCodes statuscodes
    let errorRightPad = max (V.imageWidth (errorAndStatResult ^. T.imageL) - V.imageWidth (errorResult ^. T.imageL)) 0 + equalPad
    let statRightPad = max (V.imageWidth (errorAndStatResult ^. T.imageL) - V.imageWidth (statResult ^. T.imageL)) 0 + equalPad
    otherResult <- T.render $ W.drawOtherStats myotherstats
    let paramBottomPad = getBottomPaddingAmt paramResult curHeight
    let latencyBottomPad = getBottomPaddingAmt latencyResult curHeight
    let bytesBottomPad = getBottomPaddingAmt bytesResult curHeight
    let errorBottomPad = getBottomPaddingAmt errorAndStatResult curHeight `div` 2
    let statCodeBottomPad = getBottomPaddingAmt errorAndStatResult curHeight - errorBottomPad
    let otherBottomPad = getBottomPaddingAmt otherResult curHeight
    T.render
      ( hBox
          [ W.drawBorder "Params" $ padBottom (Pad paramBottomPad) $ padRight (Pad equalPad) $ W.drawParams myparams,
            W.drawBorder "Latency Stats(s)" $ padBottom (Pad latencyBottomPad) $ padRight (Pad equalPad) $ W.drawLatencyStats mylatencies,
            W.drawBorder "Bytes" $ padBottom (Pad bytesBottomPad) $ padRight (Pad equalPad) $ W.drawBytes bytes,
            vBox
              [ W.drawBorder "Status Codes" $ padBottom (Pad statCodeBottomPad) $ padRight (Pad statRightPad) $ W.drawStatusCodes statuscodes,
                W.drawBorder "Errors" $ padBottom (Pad errorBottomPad) $ padRight (Pad errorRightPad) $ W.drawErrors errors
              ],
            W.drawBorder "Other Stats" $ padBottom (Pad otherBottomPad) $ padRight Max $ W.drawOtherStats myotherstats
          ]
      )

-- | UI Event handler. Listens for new AttackResultMessage events and updates the AppState
handleEvent :: T.BrickEvent Name (Either Utils.Models.AttackResultMessage Float) -> T.EventM Name AppState ()
handleEvent e = case e of
  (T.AppEvent (Right f)) -> do
    numDone' <- use numDone
    pbState %= (\_ -> if numDone' == 0 then 0 else min f 1.0)
  (T.AppEvent (Left (ResultMessage newAttackResult))) -> do
    latencies %= (++ [latency newAttackResult])

    numDone += 1
    numDone' <- use numDone

    Control.Monad.when (code newAttackResult == 200) $ numSuccess += 1

    numSuccess' <- use numSuccess

    let newBytesIn = bytesIn newAttackResult
        newBytesOut = bytesOut newAttackResult
     in bytesMetrics %= updatedByteMetrics newBytesIn newBytesOut numDone'

    let newCode = show (code newAttackResult)
     in statusCodes %= \x -> updateStatusCode x newCode

    otherstats %= updateOtherStats numDone' numSuccess' newAttackResult

    case Utils.Models.error newAttackResult of
      Just err -> reqErrors %= (\(W.MkErrors e) -> W.MkErrors $ Set.insert withoutQuotes e)
        where
          withoutQuotes = filter (/= '"') err
      Nothing -> return ()
  (T.VtyEvent (V.EvKey (V.KChar 'q') [])) -> do
    M.halt
    liftIO exitSuccess
  _ -> return ()

-- | Updates the byte metrics widget with new entries
updatedByteMetrics :: Integer -> Integer -> Int -> BytesWidget -> BytesWidget
updatedByteMetrics newBytesIn newBytesOut numDone' (W.MkBytesWidget i o) =
  W.MkBytesWidget
    { W.inMetrics =
        W.MkBytesMetrics
          { W.totalB = W.totalB i + newBytesIn,
            W.meanB = fromIntegral (W.totalB i + newBytesIn) / fromIntegral numDone'
          },
      W.outMetrics =
        W.MkBytesMetrics
          { W.totalB = W.totalB o + newBytesOut,
            W.meanB = fromIntegral (W.totalB o + newBytesOut) / fromIntegral numDone'
          }
    }

-- | Updates the other statistics widget with new entries
updateOtherStats :: Int -> Int -> AttackResult -> OtherStats -> OtherStats
updateOtherStats numHits numSuccess result oldStats =
  oldStats
    { requests = numHits,
      success = fromIntegral numSuccess / fromIntegral numHits,
      earliest = earliest,
      latest = Utils.Models.requestTimestamp result,
      end = Utils.Models.responseTimestamp result,
      throughput = fromIntegral numSuccess / realToFrac (diffUTCTime latest earliest)
    }
  where
    earliest =
      if numHits == 1
        then Utils.Models.requestTimestamp result
        else W.earliest oldStats
    latest = Utils.Models.responseTimestamp result

-- | Updates the status code widget with new entries
updateStatusCode :: W.StatusCodes -> String -> W.StatusCodes
updateStatusCode (W.MkStatusCodes codes) key = W.MkStatusCodes updatedCodes
  where
    updatedCodes = alter updateValue key codes
    updateValue :: Maybe Int -> Maybe Int
    updateValue (Just x) = Just (x + 1)
    updateValue Nothing = Just 1
