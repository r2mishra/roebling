--- Reference: https://github.com/madnight/asciichart
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- needed for makelenses
{-# LANGUAGE TemplateHaskell #-}

module Chart
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
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import Control.Concurrent.STM (stateTVar)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST.Safe (STArray, getElems, newArray, writeArray)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, unfoldr)
import Data.Time (NominalDiffTime)
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import Text.Printf (printf)
import Widgets (BytesWidget)
import qualified Widgets as W

data Options = MkOptions
  { -- | Allows to set the height of the chart.
    height :: Int
  }

-- -- | Provides default options: @Options { 'height' = 14 }@.
-- options :: Options
-- options =
--   MkOptions {height = 14}

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

plotWith' :: Options -> [Double] -> [String]
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
-- --   corresponding chart with a default terminal height of 14 blocks.
-- plot :: [Double] -> IO ()
-- plot x = if length x < 1 then return () else plotWith options x

-- | Same as plot but it's possible to define custom options.
--   Example: @'plotWith' options { 'height' = 20 }@
plotWith :: Options -> [Double] -> IO ()
plotWith options' series =
  forM_ result $
    putStrLn . dropWhileEnd isSpace . concat
  where
    result = splitEvery (length series + 4) $ plotWith' options' series

-- TODO: what's the magic 4 number used by asciichart?
getPlotLines :: Options -> [Double] -> [String]
getPlotLines options' series = map (dropWhileEnd isSpace . concat) result
  where
    result = splitEvery (length series + 4) $ plotWith' options' series

type Name = ()

data AppState = AppState
  { _latencies :: [NominalDiffTime],
    _numDone :: Int, -- current progress
    _hitCount :: Int, -- total number needed
    _bytesMetrics :: BytesWidget,
    _plotOptions :: Options,
    _params :: W.Params,
    _statusCodes :: W.StatusCodes,
    _reqErrors :: W.Errors,
    _otherstats :: W.OtherStats
    -- Include other fields as necessary
  }

makeLenses ''AppState -- provides a convenient way to access state vars while handling events

-- | The main Brick application
plotApp :: M.App AppState [NominalDiffTime] Name
plotApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

-- TODO: Dummy attribute map for now. Can add colors etc here
theMap :: Brick.AttrMap.AttrMap
theMap = Brick.AttrMap.attrMap V.defAttr []

-- | The plotting Widget
plotWidget :: Options -> [Double] -> T.Widget n
plotWidget myoptions mylatencies =
  joinBorders $
    withBorderStyle unicode $
      borderWithLabel (str "Latencies") $
        str (unlines $ getPlotLines myoptions mylatencies)

-- | Final combined UI with all the Widgets
drawUI :: AppState -> [T.Widget ()]
drawUI state = [go]
  where
    go = ui myparams myoptions mylatencies mybytes mystatuscodes myerrors myotherstats
    myparams = _params state
    myoptions = _plotOptions state
    mylatencies = _latencies state
    mybytes = _bytesMetrics state
    mystatuscodes = _statusCodes state
    myerrors = _reqErrors state
    myotherstats = _otherstats state

-- The UI widget that includes the ASCII chart
ui :: W.Params -> Options -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes -> W.Errors -> W.OtherStats -> T.Widget ()
ui myparams myoptions mylatencies bytes statuscodes errors myotherstats =
  vBox
    [ plotWidget myoptions (map realToFrac mylatencies :: [Double]),
      hBox
        [ W.drawParams myparams,
          W.drawLatencyStats mylatencies,
          W.drawBytes bytes,
          vBox
            [ W.drawStatusCodes statuscodes,
              W.drawErrors errors
            ],
          W.drawOtherStats myotherstats
        ]
    ]

-- TODO: Currently, an event is either a keyboard entry or a list of latencies. This should include other data like OtherStats, etc.
handleEvent :: T.BrickEvent Name [NominalDiffTime] -> T.EventM Name AppState ()
handleEvent e = case e of
  (T.AppEvent newLatencies) -> latencies %= const newLatencies
  (T.VtyEvent (V.EvKey (V.KChar 'q') [])) -> M.halt
  _ -> return ()
