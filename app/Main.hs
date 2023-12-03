{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (unpack)
import Options.Applicative
import Data.Text (unpack)
import Data.Time (NominalDiffTime)
import Chart (Options(..), getPlotLines)
import Control.Monad (when)

import Brick.Widgets.Border.Style (unicode)
import Brick
import Brick.Widgets.Border

import Lib
import qualified Widgets as W
import Args


-- Sample data for the chart
mySeries :: [Integer]
mySeries = [1..20]

myLatencies :: [NominalDiffTime]
myLatencies = map fromRational [0.8, 0.7, 0.98, 0.55, 0.66]

myoptions :: Options 
myoptions  = MkOptions { height = 14 }

plotWidget :: Widget n
plotWidget = joinBorders $
    withBorderStyle unicode $
    borderWithLabel (Brick.str "Plot") $
    Brick.str (unlines $ getPlotLines myoptions mySeries)

-- The UI widget that includes the ASCII chart
ui :: W.ParamsText -> [NominalDiffTime] -> Widget ()
ui params latencies = vBox [plotWidget, hBox [W.drawParams params, W.drawLatencyStats latencies]]


main :: IO ()
main = do
    cmdFlags <- execParser (info (helper <*> flags) fullDesc)
    print cmdFlags

    when (plotDemo cmdFlags) $ do
        let params = W.MakeParams {
            W.target = target cmdFlags, 
            W.rate = rate cmdFlags,
            W.duration = duration cmdFlags,
            W.method = method cmdFlags
        }
        simpleMain $ ui params myLatencies

    attacker (unpack $ target cmdFlags)
    putStrLn $ "Attacking " ++ show (target cmdFlags)
