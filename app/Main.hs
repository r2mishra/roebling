{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Args
import Lib
import Options.Applicative
import Data.Text (unpack)
import Chart (plotWith, options)
import Control.Monad (when)

main :: IO ()
main = do
    putStrLn "I Looooove everythign about this place"
    cmdFlags <- execParser (info (helper <*> flags) fullDesc)
    print cmdFlags
    attacker (unpack $ target cmdFlags)
    putStrLn $ "Attacking " ++ show (target cmdFlags)


    -- when (plotDemo flags) $ do 
    --     let plotSeries = [1..20]
    --     plotWith options plotSeries


