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
import GUI.SampleData
import qualified GUI.Widgets as W
import Options.Applicative
import System.IO
import qualified Utils.Models as Models
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)

  let targetter = buildTargetter cmdFlags
  let pacer = buildPacer cmdFlags
  attackChannel <- newChan

  putStrLn "Press Enter to start the attack"
  _ <- getLine

  attackerThread <- async $ runAttacker attackChannel targetter pacer

  -- appendFile "results.log" ("Log for attacking url:" ++ (show (Args.target cmdFlags)) ++ "\n")
  -- fetcherThread <- async $ runLogger "results.log" attackChannel

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
initializeAndRunPlot :: Flags -> Chan Models.AttackResultMessage -> IO ()
initializeAndRunPlot cmdFlags chan = do
  let params =
        W.MkParams
          { W.target = target cmdFlags,
            W.rate = rate cmdFlags,
            W.duration = duration cmdFlags,
            W.method = method cmdFlags
          }

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
            _numSuccess = 0

          }
  bchan <- newBChan 10000
  -- updates latencies in a new thread
  _ <- forkIO $ chanToBChanAdapter chan bchan
  -- _ <- tick (fromIntegral (duration cmdFlags)) bchan
  -- TODO: this can be run in it's own thread as well.
  void $ M.customMainWithDefaultVty (Just bchan) plotApp initialState

chanToBChanAdapter :: Chan Models.AttackResultMessage -> BChan (Either Models.AttackResultMessage Float) -> IO ()
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
      writeBChan chan (Right (f / fromIntegral dur))
      threadDelay 100000
      go (f + 0.1)
