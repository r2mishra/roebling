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
import Data.Time.Clock.System (getSystemTime, systemSeconds)
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
  currTime <- getSystemTime
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
            _hitCount = (duration cmdFlags) * (rate cmdFlags),
            _pbState = 0.0,
            _startTime = fromIntegral (systemSeconds currTime)
          }
  bchan <- newBChan 100
  -- updates latencies in a new thread
  _ <- forkIO $ chanToBChanAdapter chan bchan
  _ <- tick (fromIntegral (duration cmdFlags)) bchan
  -- TODO: this can be run in it's own thread as well.
  void $ M.customMainWithDefaultVty (Just bchan) plotApp initialState

chanToBChanAdapter :: Chan  Models.AttackResultMessage -> BChan (Either Models.AttackResultMessage Float) -> IO ()
chanToBChanAdapter inputChan outputBChan = loop
  where
    loop = do
      message <- readChan inputChan
      writeBChan outputBChan (Left message)
      loop

tick :: Integer -> BChan (Either Models.AttackResultMessage Float) -> IO GHC.Conc.Sync.ThreadId
tick dur chan = forkIO $ go 0.0
  where
    go f = do
      writeBChan chan (Right (f/ fromIntegral dur))
      threadDelay 100000
      go (f + 0.1)

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
      threadDelay 1000000
      go newLatencies
