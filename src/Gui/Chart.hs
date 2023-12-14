--- Reference: https://github.com/madnight/asciichart
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- needed for makelenses
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Foldable (toList)
import Data.List (dropWhileEnd, foldl', unfoldr)
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as TL
import Data.Time (NominalDiffTime)
import Debug.Trace
import GUI.Widgets (BytesWidget)
import qualified GUI.Widgets as W
import qualified Graphics.Vty
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import System.Console.Terminal.Size (size, width)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

appendDebugLog :: String -> IO ()
appendDebugLog msg = appendFile "debug.log" (msg ++ "\n")

-- import Linear (downsample)
-- import DSP.Basic (downsample)

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
    _termwidth :: Int,
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

-- -- | The plotting Widget
-- plotWidget :: Options -> [Double] -> T.Widget n
-- plotWidget myoptions mylatencies =
--   joinBorders $
--     withBorderStyle unicode $
--       borderWithLabel (str "Latencies") $
--         str (unlines $ getPlotLines myoptions mylatencies)

-- BIGGEST HEADACHE
myFillPlotWidget :: Int -> Options -> [Double] -> T.Widget n
myFillPlotWidget term_width myoptions mylatencies =
  joinBorders $
    withBorderStyle
      unicode
      ( borderWithLabel
          (str "Latencies")
          internalWidget
      )
  where
    internalWidget = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let a = ctx ^. (T.attrL)
      let curWidth = round (0.1 * fromIntegral term_width)
      let cur_strings = getPlotLines myoptions mylatencies
      let cur_string_width = textWidth (head cur_strings)
      let newLatencies = if cur_string_width > curWidth
            then resizeStringList mylatencies cur_string_width curWidth
            else mylatencies
      let max_num_width = length (printf "%0.2f" (realToFrac $ maximum mylatencies :: Float) :: String)
      -- let newStrings = [keepLabelAndLastN' max_num_width curWidth x | x <- cur_strings] -- this is working. updating latencies isn't (??)
      let newStrings = getPlotLines myoptions newLatencies
      let plotLines = map TL.pack newStrings
      let image = V.vertCat $ map (V.text V.defAttr) plotLines
      return $
        T.Result
          image
          []
          []
          []
          Brick.BorderMap.empty


keepLabelAndLastN' :: Int -> Int -> [a] -> [a]
keepLabelAndLastN' skipNum n xs = (take skipNum xs) ++ (lastN' n (lastN' (length xs - skipNum) xs))

-- >>> keepLabelAndLastN' 3 0 [1,2, 3,4,5,6,7,8,9]
-- [1,2,3]

resizeStringList :: [Double] -> Int -> Int -> [Double]
-- resizeStringList mylatencies cur_string_width curWidth = downsample frac mylatencies
--     where frac = fromIntegral curWidth / fromIntegral cur_string_width
resizeStringList mylatencies cur_string_width curWidth = lastN' subN mylatencies
  where
    subN = round (fromIntegral (length mylatencies) * 0.9)
    -- subN = round (fromIntegral (length mylatencies) * (fromIntegral curWidth / fromIntegral cur_string_width))


lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _ a = a

-- NOT USED RN
downsample :: (RealFrac a) => a -> [b] -> [b]
downsample frac lst
  | frac >= 1 = lst
  | otherwise = go 0 0
  where
    len = fromIntegral $ length lst
    num_out = max (round (len * frac)) 1
    step = round (len / fromIntegral num_out)
    go i n
      | i >= length lst = []
      | n <= i = lst !! i : go (i + 1) (n + step)
      | otherwise = go (i + 1) n

-- | Final combined UI with all the Widgets
drawUI :: AppState -> [T.Widget ()]
drawUI state = [go]
  where
    go = ui mytermwidth myparams myoptions mylatencies mybytes mystatuscodes myerrors myotherstats
    mytermwidth = _termwidth state
    myparams = _params state
    myoptions = _plotOptions state
    mylatencies = _latencies state
    mybytes = _bytesMetrics state
    mystatuscodes = _statusCodes state
    myerrors = _reqErrors state
    myotherstats = _otherstats state

-- The UI widget that includes the ASCII chart
ui :: Int -> W.Params -> Options -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes -> W.Errors -> W.OtherStats -> T.Widget ()
ui termwidth myparams myoptions mylatencies bytes statuscodes errors myotherstats =
  vBox
    [ myFillPlotWidget termwidth myoptions (map realToFrac mylatencies :: [Double]),
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
