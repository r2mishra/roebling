{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Args
import qualified Args
import Attacker (runAttacker)
import Brick
import qualified Brick.Main as M
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Chart (Options (..), getPlotLines)
import Control.Concurrent (newChan)
import Control.Concurrent.Async
import Control.Monad
import Data.Text (unpack)
import Data.Time (NominalDiffTime)
import Options.Applicative
import qualified Pacer
import ResultLogger (runLogger)
import Targeter (Target (..))
import qualified Widgets as W
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

import ProgressBar
import SampleData
import Widgets (BytesMetrics)

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)
  when (plotDemo cmdFlags) $ do
    let params =
          W.MkParams
            { W.target = target cmdFlags,
              W.rate = rate cmdFlags,
              W.duration = duration cmdFlags,
              W.method = method cmdFlags
            }
    simpleMain $ ui params myLatencies myBytes myStatusCodes myErrors myOtherStats

  when (progressBar cmdFlags) $ do
    void $ M.defaultMain theApp initialState
  let targetter = buildTargetter cmdFlags
  let pacer = buildPacer cmdFlags
  attackChannel <- newChan
  attackerThread <- async $ runAttacker attackChannel targetter pacer
  fetcherThread <- async $ runLogger attackChannel
  wait attackerThread
  wait fetcherThread

buildTargetter :: Args.Flags -> Target
buildTargetter cmdFlags =
  Target
    { url = unpack (Args.target cmdFlags),
      verb = Args.method cmdFlags,
      Targeter.body = Just [Args.body cmdFlags],
      Targeter.headers = [("Content-Type", "application/json")]
    }

buildPacer :: Args.Flags -> Pacer.PaceConfig
buildPacer cmdFlags =
  Pacer.PaceConfig
    { Pacer.rate = Args.rate cmdFlags,
      Pacer.duration = fromIntegral (Args.duration cmdFlags)
    }

plotWidget :: Widget n
plotWidget =
  joinBorders $
    withBorderStyle unicode $
      borderWithLabel (Brick.str "Plot") $
        Brick.str (unlines $ getPlotLines myoptions mySeries)

-- The UI widget that includes the ASCII chart
ui :: W.Params -> [NominalDiffTime] -> W.BytesWidget -> W.StatusCodes -> W.Errors -> W.OtherStats -> Widget ()
ui params latencies bytes statuscodes errors otherstats =
  vBox
    [ plotWidget,
      hBox
        [ W.drawParams params,
          W.drawLatencyStats latencies,
          W.drawBytes bytes,
          vBox
            [ W.drawStatusCodes statuscodes,
              W.drawErrors errors
            ],
          W.drawOtherStats otherstats
        ]
    ]
