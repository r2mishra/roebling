{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Text (unpack)
import Options.Applicative
import Data.Time (NominalDiffTime)
import Chart (Options(..), getPlotLines)

import Lib
import qualified Widgets as W
import Args

import Brick
import qualified Brick.Main as M
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Control.Monad 
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

import ProgressBar
import Widgets (BytesMetrics)
import SampleData 

plotWidget :: Widget n
plotWidget = joinBorders $
    withBorderStyle unicode $
    borderWithLabel (Brick.str "Plot") $
    Brick.str (unlines $ getPlotLines myoptions mySeries)

-- The UI widget that includes the ASCII chart
ui :: W.Params -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes -> W.Errors -> W.OtherStats -> Widget ()
ui params latencies bytes statuscodes errors otherstats = vBox [
    plotWidget, 
    hBox [
        W.drawParams params, 
        W.drawLatencyStats latencies,
        W.drawBytes bytes,
        vBox [
            W.drawStatusCodes statuscodes,
            W.drawErrors errors
        ],
        W.drawOtherStats otherstats
        ]
    ]

main :: IO ()
main = do
    cmdFlags <- execParser (info (helper <*> flags) fullDesc)
    print cmdFlags

    when (plotDemo cmdFlags) $ do
        let params = W.MkParams {
            W.target = target cmdFlags, 
            W.rate = rate cmdFlags,
            W.duration = duration cmdFlags,
            W.method = method cmdFlags
        }
        simpleMain $ ui params myLatencies myBytes myStatusCodes myErrors myOtherStats

    when (progressBar cmdFlags) $ do  
      void $ M.defaultMain theApp initialState

    attacker (unpack $ target cmdFlags)
    putStrLn $ "Attacking " ++ show (target cmdFlags)
