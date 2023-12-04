{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Text (unpack)
import Options.Applicative
import Data.Text (unpack)
import Data.Time (NominalDiffTime)
import Chart (Options(..), getPlotLines)
import Control.Monad (when)

import Lib
import qualified Widgets as W
import Args

import Brick
import qualified Brick.Widgets.ProgressBar as P
import qualified Brick.Main as M
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import qualified Brick.Types as T

import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>), (<=>)
  , str
  , updateAttrMap
  , overrideAttr
  )
import Brick.Util (fg, bg, on, clamp)

import Control.Monad 

import qualified Graphics.Vty as V

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

import Lens.Micro.Mtl
import Lens.Micro.TH

import ProgressBar


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

    when (progressBar cmdFlags) $ do  
      void $ M.defaultMain theApp initialState

    attacker (unpack $ target cmdFlags)
    putStrLn $ "Attacking " ++ show (target cmdFlags)
