{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}

module Chart
    ( -- * Plot
      plot
    , plotWith
    , plotWith'
      -- * Options
    , options
    , height
    ) where

import Control.Monad.ST        (ST, runST)
import Control.Monad           (forM_)
import Data.Array.ST.Safe      (STArray, getElems, writeArray, newArray)
import Data.Char               (isSpace)
import Data.List               (unfoldr, dropWhileEnd)
import Text.Printf             (printf)
import Data.Bool               (bool)

data Options =
  Options { height :: Int  -- ^ Allows to set the height of the chart.
          }

-- | Provides default options: @Options { 'height' = 14 }@.
options :: Options
options =
  Options { height = 14 }

newArray2D :: Integer -> Integer ->
              ST s (STArray s (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

pad :: Integral a => [a] -> Int
pad series =
  let floats = fromIntegral <$> series
      toStr :: [Float] -> [String]
      toStr = fmap (printf "%0.2f")
  in  maximum $ length <$> toStr floats

plotWith' :: Options -> [Integer] -> [String]
plotWith' opts series =

    -- variables and functions
    let min' = minimum series
        max' = maximum series
        range = abs $ max' - min'
        offset = 3
        ratio = if range == 0 then 1
                else fromIntegral (height opts) / fromIntegral range :: Float
        min2 = fromIntegral min' * ratio
        max2 = fromIntegral max' * ratio
        rows = round $ abs $ max2 - min2
        width = toInteger $ length series + 3

    in runST $ do

    -- array creation
    arr <- newArray2D rows width
    let result x y = writeArray arr (head x, head y)

    -- axis and labels
    forM_ [min2..max2] $ \y -> do
            let label = if rows == 0 then y
                        else fromInteger max' - (y - min2) *
                             fromInteger range / fromIntegral rows
            result [round $ y - min2] [max 0 $ offset - 5] $
                   printf ("%" ++ show (pad series) ++ ".2f") label
            result [round $ y - min2] [offset - 1] . bool "┤" "┼" $ y == 0

    -- initial value
    let first = fromInteger (head series) * ratio - min2
    result [round $ fromInteger rows - first] [offset - 1] "┼"

    -- plot the line
    forM_ [0..length series - 2] $ \x -> do
        let offset' = toInteger x + offset
        let y' i = round (fromInteger (series !! i) * ratio) - round min2
        let (y0, y1) = (y' x, y' $ x + 1)
        if y0 == y1 then
            result [rows - y0] [offset'] "─"
        else do
            result [rows - y1] [offset'] . bool "╭" "╰" $ y0 > y1
            result [rows - y0] [offset'] . bool "╯" "╮" $ y0 > y1

            forM_ [min y0 y1 + 1..max y0 y1 - 1] $ \y ->
                result [rows - y] [offset'] "│"

    getElems arr

-- | Takes a List of Integers and prints out a
--   corresponding chart with a default terminal height of 14 blocks.
plot :: [Integer] -> IO ()
plot x =  if length x < 1 then return () else plotWith options x

-- | Same as plot but it's possible to define custom options.
--   Example: @'plotWith' options { 'height' = 20 }@
plotWith :: Options -> [Integer] -> IO ()
plotWith options' series = forM_ result $
      putStrLn . dropWhileEnd isSpace . concat
    where result = splitEvery (length series + 4) $ plotWith' options' series

-- >>> plotWith' options [1..20]
-- ["20.00"," ","\9508"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ","\9581"," "," ","18.64"," ","\9508"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ","\9581","\9583"," "," ","17.29"," ","\9508"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ","\9581","\9472","\9583"," "," "," ","15.93"," ","\9508"," "," "," "," "," "," "," "," "," "," "," "," "," "," ","\9581","\9583"," "," "," "," "," ","14.57"," ","\9508"," "," "," "," "," "," "," "," "," "," "," "," "," ","\9581","\9583"," "," "," "," "," "," ","13.21"," ","\9508"," "," "," "," "," "," "," "," "," "," "," ","\9581","\9472","\9583"," "," "," "," "," "," "," ","11.86"," ","\9508"," "," "," "," "," "," "," "," "," "," ","\9581","\9583"," "," "," "," "," "," "," "," "," ","10.50"," ","\9508"," "," "," "," "," "," "," "," "," ","\9581","\9583"," "," "," "," "," "," "," "," "," "," "," 9.14"," ","\9508"," "," "," "," "," "," "," ","\9581","\9472","\9583"," "," "," "," "," "," "," "," "," "," "," "," 7.79"," ","\9508"," "," "," "," "," "," ","\9581","\9583"," "," "," "," "," "," "," "," "," "," "," "," "," "," 6.43"," ","\9508"," "," "," "," "," ","\9581","\9583"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," 5.07"," ","\9508"," "," "," ","\9581","\9472","\9583"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," 3.71"," ","\9508"," "," ","\9581","\9583"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," 2.36"," ","\9508"," ","\9581","\9583"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," 1.00"," ","\9532","\9472","\9583"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "]
