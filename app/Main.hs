{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Args
import Data.Text (unpack)
import Lib
import Options.Applicative
import Attacker (runAttacker)
import qualified Pacer

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)
  print cmdFlags
  let paceConfig = Pacer.PaceConfig (Args.rate cmdFlags) (fromIntegral (Args.duration cmdFlags))
  runAttacker paceConfig "src/results.txt"
  putStrLn $ "Attacking " ++ show (Args.target cmdFlags)
