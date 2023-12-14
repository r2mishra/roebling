{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args
import Attacker.Attacker (runAttacker)
import qualified Attacker.Pacer as Pacer
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
import System.Console.Terminal.Size (size, width)
import System.IO
import qualified Utils.Models as Models

-- Somewhere in your initialization code
setupDebugLog :: IO ()
setupDebugLog = writeFile "debug.log" "Starting Debug Log\n"

main :: IO ()
main = do
  setupDebugLog -- DEBUGGING
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)
  -- TODO: plotting is still sequential, uses only dummy data

  let targetter = buildTargetter cmdFlags
  let pacer = buildPacer cmdFlags
  attackChannel <- newChan

  putStrLn "Press Enter to start the attack"
  _ <- getLine

  attackerThread <- async $ runAttacker attackChannel targetter pacer
  -- fetcherThread <- async $ runLogger attackChannel

  initializeAndRunPlot cmdFlags attackChannel
  wait attackerThread

-- wait fetcherThread

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
initializeAndRunPlot :: Flags -> Chan Models.AttackResultMessage -> IO ()
initializeAndRunPlot cmdFlags chan = do
  -- get terminal width
  maybeTermWidth <- size
  let termwidth = case maybeTermWidth of
        Just windowsize -> width windowsize
        Nothing -> 80 -- a random default
  print ("Term width: " ++ show termwidth)
  -- _ <- putStrLn "Term width: " ++ termwidth
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
            _pbState = W.initialPBState,
            _termwidth = termwidth
            _pbState = 0.0
          }
  bchan <- newBChan 100
  -- updates latencies in a new thread
  _ <- forkIO $ chanToBChanAdapter chan bchan
  -- TODO: this can be run in it's own thread as well.
  void $ M.customMainWithDefaultVty (Just bchan) plotApp initialState

chanToBChanAdapter :: Chan Models.AttackResultMessage -> BChan Models.AttackResultMessage -> IO ()
chanToBChanAdapter inputChan outputBChan = loop
  where
    loop = do
      message <- readChan inputChan
      writeBChan outputBChan message
      loop

-- TODO: Currently, this only updates the latencies. Should also allow updates for OtherStats, etc
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
      threadDelay 10000
      go newLatencies
