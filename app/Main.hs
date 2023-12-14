{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args
import Attacker.ResultLogger (runLogger)
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Main as M
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Monad
import Data.Time (NominalDiffTime)
import GHC.Conc.IO (threadDelay)
import qualified GHC.Conc.Sync
import GUI.Chart
import GUI.ProgressBar
import GUI.SampleData
import qualified GUI.Widgets as W
import Options.Applicative
import qualified Utils.Models as Models

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)
  attackChannel <- newChan
  initializeAndRunPlot cmdFlags attackChannel

-- Implement the logic to read from the channel and update the graph
-- Currently, this uses dummy data, can be extended to use data from the attacker
initializeAndRunPlot :: Flags -> Chan Models.AttackResultMessage -> IO ()
initializeAndRunPlot cmdFlags chan = do
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
            _hitCount = 0,
            _pbState = 0.0,
            _cmdFlags = cmdFlags,
            _attackChan = chan
          }
  bchan <- newBChan 100
  _ <- forkIO $ chanToBChanAdapter chan bchan
  void $ M.customMainWithDefaultVty (Just bchan) plotApp initialState

chanToBChanAdapter :: Chan Models.AttackResultMessage -> BChan Models.AttackResultMessage -> IO ()
chanToBChanAdapter inputChan outputBChan = loop
  where
    loop = do
      message <- readChan inputChan
      writeBChan outputBChan message
      loop
