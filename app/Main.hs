{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Args
import qualified Args
import Attacker.Attacker (runAttacker)
import qualified Attacker.Pacer as Pacer
import Attacker.ResultLogger (runLogger)
import qualified Attacker.Targeter as Targeter
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Main as M
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Concurrent (Chan, forkIO, newChan, readChan, threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Data.Monoid
import Data.Text (unpack)
import Data.Time (NominalDiffTime)
import qualified GHC.Conc.Sync
import GUI.Chart (AppState (..), Options (..), getPlotLines, plotApp)
import GUI.ProgressBar
import GUI.SampleData
import qualified GUI.Widgets as W
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform
import Options.Applicative
import qualified Utils.Models as Models

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)
  -- TODO: plotting is still sequential, uses only dummy data
  when (plotDemo cmdFlags) $ initializeAndRunPlot cmdFlags

  when (progressBar cmdFlags) $ do
    void $ M.defaultMain theApp initialPBState

  let targetter = buildTargetter cmdFlags
  let pacer = buildPacer cmdFlags
  attackChannel <- newChan

  putStrLn "Press Enter to start the attack"
  _ <- getLine
  
  attackerThread <- async $ runAttacker attackChannel targetter pacer
  fetcherThread <- async $ runLogger attackChannel
  -- graphThread <- async $ updatePlot attackChannel
  wait attackerThread
  wait fetcherThread

buildTargetter :: Args.Flags -> Models.Target
buildTargetter cmdFlags =
  Models.Target
    { Models.url = Args.target cmdFlags,
      Models.verb = Args.method cmdFlags,
      Models.body = Args.body cmdFlags,
      Models.bodyFile = Args.bodyFile cmdFlags,
      Models.headers = [("Content-Type", "application/json")]
    }

buildPacer :: Args.Flags -> Pacer.PaceConfig
buildPacer cmdFlags =
  Pacer.PaceConfig
    { Pacer.rate = Args.rate cmdFlags,
      Pacer.duration = fromIntegral (Args.duration cmdFlags)
    }

-- Implement the logic to read from the channel and update the graph
-- Currently, this uses dummy data, can be extended to use data from the attacker
initializeAndRunPlot :: Flags -> IO ()
initializeAndRunPlot cmdFlags = do
  let params =
        W.MkParams
          { W.target = target cmdFlags,
            W.rate = rate cmdFlags,
            W.duration = duration cmdFlags,
            W.method = method cmdFlags
          }
      -- initial state with dummy data.
      -- TODO: latencies should be initialized as empty
      initialState =
        AppState
          { _params = params,
            _plotOptions = myoptions,
            _latencies = myLatencies,
            _bytesMetrics = myBytes,
            _statusCodes = myStatusCodes,
            _reqErrors = myErrors,
            _otherstats = myOtherStats,
            _numDone = 0,
            _hitCount = 0
          }
  chan <- newBChan 10
  -- updates latencies in a new thread
  _ <- sendLatencies myLatencies chan
  -- TODO: this can be run in it's own thread as well.
  void $ M.customMainWithDefaultVty (Just chan) plotApp initialState

-- TODO: Currently, this only updates the latencies. Shoudl also allow updates for OtherStats, etc
sendLatencies :: [NominalDiffTime] -> BChan [NominalDiffTime] -> IO GHC.Conc.Sync.ThreadId
sendLatencies initLatencies chan = forkIO $ go initLatencies
  where
    go latencies = do
      -- Generate or fetch new latencies
      value <- generateRandomDouble
      let newLatencies = latencies ++ [realToFrac value]

      -- Write the new latencies to the channel
      writeBChan chan newLatencies

      -- Wait for some time before sending the next update
      threadDelay 1000000
      go newLatencies
