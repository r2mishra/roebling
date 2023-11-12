module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
    [target] <- getArgs
    attacker target
    return ()
