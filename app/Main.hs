{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Args
import Lib
import Options.Applicative
import Data.Text (unpack)

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> flags) fullDesc)
  print cmdFlags
  attacker (unpack $ target cmdFlags)
  putStrLn $ "Attacking " ++ show (target cmdFlags)
