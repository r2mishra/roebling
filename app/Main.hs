{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Args
import Data.Text (unpack)
import Lib
import Options.Applicative
import Attacker (runAttacker)
import qualified Pacer
import Control.Concurrent (newChan)
import Data.Text.Array (run)
import Control.Concurrent.Async
import ResultLogger (runLogger)

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)
  print cmdFlags
  let paceConfig = Pacer.PaceConfig (Args.rate cmdFlags) (fromIntegral (Args.duration cmdFlags))
  resultChannel <- newChan
  attackerThread <- async $ runAttacker resultChannel paceConfig
  fetcherThread <- async $ runLogger resultChannel
  putStrLn $ "Attacking " ++ show (Args.target cmdFlags)

  wait attackerThread
  wait fetcherThread