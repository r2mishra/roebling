{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Args
import Lib
import Options.Applicative
import Data.Text (unpack)
import Chart (Options(..), getPlotLines)
import Control.Monad (when)
import Brick.Widgets.Border.Style (unicode)

import Brick
import Brick.Widgets.Border

-- Sample data for the chart
mySeries :: [Integer]
mySeries = [1..20]

myoptions :: Options 
myoptions  = MkOptions { height = 14 }

-- The UI widget that includes the ASCII chart
ui :: Widget ()
ui = joinBorders $
    withBorderStyle unicode $
    borderWithLabel (Brick.str "Hello") $
    Brick.str (unlines $ getPlotLines myoptions mySeries)

main :: IO ()
main = do
    cmdFlags <- execParser (info (helper <*> flags) fullDesc)
    print cmdFlags

    when (plotDemo cmdFlags) $ do
        simpleMain ui

    attacker (unpack $ target cmdFlags)
    putStrLn $ "Attacking " ++ show (target cmdFlags)
