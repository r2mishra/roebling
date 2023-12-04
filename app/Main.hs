{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Args
import Data.Text (unpack)
import Lib
import Options.Applicative

import Brick
import qualified Brick.Widgets.ProgressBar as P
import qualified Brick.Main as M
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import qualified Brick.Types as T

import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>), (<=>)
  , str
  , updateAttrMap
  , overrideAttr
  )
import Brick.Util (fg, bg, on, clamp)

import Control.Monad 

import qualified Graphics.Vty as V

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

import Lens.Micro.Mtl
import Lens.Micro.TH

import ProgressBar

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> flags) fullDesc)
  print cmdFlags

  when (progressBarFlag cmdFlags) $ do  
    void $ M.defaultMain theApp initialState

  attacker (unpack $ target cmdFlags)
  putStrLn $ "Attacking " ++ show (target cmdFlags)
