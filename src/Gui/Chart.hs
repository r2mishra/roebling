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
import Brick.Util
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import qualified Brick.Widgets.ProgressBar as P
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
import Data.List (dropWhileEnd, unfoldr)
import Data.Set (insert)
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
import Lens.Micro ((^.), (&), (.~), to)
import Text.Printf (printf)
import Utils.Models
import Graphics.Vty (horizCat)

appendDebugLog :: String -> IO ()
appendDebugLog msg = appendFile "debug.log" (msg ++ "\n")


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
    _otherstats :: W.OtherStats,
    _pbState :: Float
    -- Include other fields as necessary
  }

makeLenses ''AppState -- provides a convenient way to access state vars while handling events

-- | The main Brick application
plotApp :: M.App AppState AttackResultMessage Name
plotApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

-- TODO: Dummy attribute map for now. Can add colors etc here
-- theMap :: Brick.AttrMap.AttrMap
-- theMap = Brick.AttrMap.attrMap V.defAttr []

theMap :: Brick.AttrMap.AttrMap
theMap =
  Brick.AttrMap.attrMap
    V.defAttr
    [ (theBaseAttr, bg V.brightBlack),
      (xDoneAttr, V.green `on` V.green),
      (xToDoAttr, V.white `on` V.black),
      (P.progressIncompleteAttr, fg V.black)
    ]

theBaseAttr :: Brick.AttrMap.AttrName
theBaseAttr = Brick.AttrMap.attrName "theBase"

xDoneAttr, xToDoAttr :: Brick.AttrMap.AttrName
xDoneAttr = theBaseAttr <> Brick.AttrMap.attrName "X:done"
xToDoAttr = theBaseAttr <> Brick.AttrMap.attrName "X:remaining"

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
          (str "Latencies(s)")
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

-- | Final combined UI with all the Widgets
drawUI :: AppState -> [T.Widget ()]
drawUI state = [go]
  where
    go = ui mytermwidth myparams myoptions mylatencies mybytes mystatuscodes myerrors myotherstats myprogressbar
    mytermwidth = _termwidth state
    myparams = _params state
    myoptions = _plotOptions state
    mylatencies = _latencies state
    mybytes = _bytesMetrics state
    mystatuscodes = _statusCodes state
    myerrors = _reqErrors state
    myotherstats = _otherstats state
    myprogressbar = _pbState state

-- The UI widget that includes the ASCII chart
ui ::Int -> W.Params -> Options -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes -> W.Errors -> W.OtherStats -> Float -> T.Widget ()
ui termwidth myparams myoptions mylatencies bytes statuscodes errors myotherstats myprogressbarstate =
  vBox
    [ myFillPlotWidget termwidth myoptions (map realToFrac mylatencies :: [Double]),
      fillWidgetsEvenly myparams mylatencies bytes statuscodes errors myotherstats,
      hBox
        [ W.drawProgressBar myprogressbarstate,
          W.drawLegend
        ]
    ]

fillWidgetsEvenly :: W.Params -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes  -> W.Errors -> W.OtherStats -> T.Widget ()
fillWidgetsEvenly myparams mylatencies bytes statuscodes errors myotherstats =
   (T.Widget T.Greedy T.Greedy $ do
                -- Compute translation offset so that loc is in the middle of the
                -- rendering area
                c <- T.getContext
                let fullWidth = c^.T.availWidthL
                    fullHeight = c^.T.availHeightL
                let indWidth = fullWidth
                let getrightPaddingAmt result maxWidth = max 0 $ maxWidth - V.imageWidth (result^.T.imageL)
                let getBottomPaddingAmt result maxHeight = max 0 $ maxHeight - V.imageHeight (result^.T.imageL)
                let getRightPadding result maxWidth = V.charFill (c^.T.attrL) ' ' (getrightPaddingAmt result maxWidth) (V.imageHeight $ result^.T.imageL) 
                let getPaddedImg result maxWidth = horizCat [result^.T.imageL, getRightPadding result maxWidth]
                curResult <- T.render $ (hBox
                  [ W.drawParams myparams,
                    W.drawLatencyStats mylatencies,
                    W.drawBytes bytes,
                    vBox
                      [ W.drawStatusCodes statuscodes,
                        W.drawErrors errors
                      ],
                    W.drawOtherStats myotherstats
                  ]
                  )
                let curHeight =  V.imageHeight (curResult^.T.imageL)
                let equalPad = getrightPaddingAmt curResult indWidth `div` 5
                latencyResult <- T.render $  W.drawLatencyStats mylatencies
                paramResult <- T.render $ W.drawParams myparams
                bytesResult <- T.render $ W.drawBytes bytes
                errorAndStatResult <- T.render $ vBox
                      [ W.drawStatusCodes statuscodes,
                        W.drawBorder "Errors" $ W.drawErrors errors
                      ]
                otherResult <- T.render $ W.drawOtherStats myotherstats
                let paramBottomPad = getBottomPaddingAmt paramResult curHeight
                let latencyBottomPad = getBottomPaddingAmt latencyResult curHeight
                let bytesBottomPad = getBottomPaddingAmt bytesResult curHeight
                let errorBottomPad = (getBottomPaddingAmt errorAndStatResult curHeight) `div` 2
                let statCodeBottomPad = (getBottomPaddingAmt errorAndStatResult curHeight) `div` 2
                let otherBottomPad = (getBottomPaddingAmt otherResult curHeight) 
                fullResult <- T.render $ (hBox [
                    W.drawBorder "Params" $ padBottom (Pad paramBottomPad) $ padRight (Pad equalPad) $ W.drawParams myparams,
                    W.drawBorder "Latency Stats(s)" $ padBottom (Pad latencyBottomPad) $ padRight (Pad equalPad) $ W.drawLatencyStats mylatencies,
                    W.drawBorder "Bytes" $ padBottom (Pad bytesBottomPad) $ padRight (Pad equalPad) $ W.drawBytes bytes,
                    vBox
                      [ W.drawBorder "Status Codes" $ padBottom (Pad statCodeBottomPad) $ padRight (Pad equalPad) $ W.drawStatusCodes statuscodes,
                        W.drawBorder "Errors" $ padBottom (Pad errorBottomPad) $  padRight (Pad equalPad) $ W.drawErrors errors
                      ],
                      W.drawBorder "Other Stats" $ padBottom (Pad otherBottomPad) $ padRight Max $ W.drawOtherStats myotherstats
                  ])
                return (fullResult)
    )
    -- ( $ T.Widget T.Greedy T.Greedy $ do
    --             c <- T.getContext
    --             let fullWidth = c^.T.availWidthL
    --                 fullHeight = c^.T.availHeightL
    --             let indWidth = fullWidth `div` 5
    --             let getrightPaddingAmt result maxWidth = max 0 $ maxWidth - V.imageWidth (result^.T.imageL)
    --             let getRightPadding result maxWidth = V.charFill (c^.T.attrL) ' ' (getrightPaddingAmt result maxWidth) (V.imageHeight $ result^.T.imageL) 
    --             let getPaddedImg result maxWidth = horizCat [result^.T.imageL, getRightPadding result maxWidth]
    --             paramResult <- T.render $ W.drawLatencyStats mylatencies
    --             let paddedParamsImage = getPaddedImg paramResult indWidth
    --             return (paramResult & (T.imageL .~ paddedParamsImage))
    -- )
              --  getrightPaddingAmt
                -- return $ paddedParams
                -- let rightPaddingAmt = max 0 $ c^.availWidthL - imageWidth (result^.imageL)
                --   bottomPaddingAmt = max 0 $ c^.availHeightL - imageHeight (result^.imageL)
                --   rightPadding = charFill (c^.attrL) ' ' rightPaddingAmt (imageHeight $ result^.imageL)
                --   bottomPadding = charFill (c^.attrL) ' ' (imageWidth $ result^.imageL) bottomPaddingAmt
                --   paddedImg = horizCat [vertCat [result^.imageL, bottomPadding], rightPadding]
                -- return $ result & T.imageL .~ paddedImg
    -- hBox [ W.drawParams myparams,
    --       W.drawLatencyStats mylatencies,
    --       W.drawBytes bytes,
    --       vBox
    --         [ W.drawStatusCodes statuscodes,
    --           W.drawErrors errors
    --         ],
    --       W.drawOtherStats myotherstats
    --     ]

-- TODO: Currently, an event is either a keyboard entry or a list of latencies. This should include other data like OtherStats, etc.
handleEvent :: T.BrickEvent Name Utils.Models.AttackResultMessage -> T.EventM Name AppState ()
handleEvent e = case e of
  (T.AppEvent (ResultMessage newAttackResult)) -> do
    latencies %= (++ [latency newAttackResult])

    numDone += 1
    numDone' <- use numDone

    hitCount' <- use hitCount

    -- pbState %= (\_ -> (fromIntegral numDone') / (fromIntegral hitCount'))
    -- TODO: Change to actual done percent
    pbState += 0.02

    case Utils.Models.error newAttackResult of
      Just err -> reqErrors %= (\(W.MkErrors e) -> W.MkErrors $ insert err e)
      Nothing -> return ()
  (T.VtyEvent (V.EvKey (V.KChar 'q') [])) -> M.halt
  _ -> return ()
