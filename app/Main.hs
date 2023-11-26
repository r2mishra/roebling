{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args
import Data.Text (unpack)
import Lib
import Options.Applicative

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> flags) fullDesc)
  print cmdFlags
  attacker (unpack $ target cmdFlags)
  putStrLn $ "Attacking " ++ show (target cmdFlags)
