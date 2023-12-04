{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module ProgressBar where

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

import Lens.Micro.Mtl
import Lens.Micro.TH

data MyAppState n = MyAppState { _x :: Float }

makeLenses ''MyAppState

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
    where
      -- use mapAttrNames
      xBar = updateAttrMap
             (A.mapAttrNames [ (xDoneAttr, P.progressCompleteAttr)
                             , (xToDoAttr, P.progressIncompleteAttr)
                             ]
             ) $ bar $ _x p
      lbl c = Just $ show $ fromEnum $ c * 100
      bar v = P.progressBar (lbl v) v
      ui = (Brick.str "X: " <+> xBar) <=>
           Brick.str "" <=>
           Brick.str "Hit 'x' to advance progress, or 'q' to quit"

-- TODO: Modify to get input from mediator
appEvent :: T.BrickEvent () e -> T.EventM () (MyAppState ()) ()
appEvent (T.VtyEvent e) =
    let valid = clamp (0.0 :: Float) 1.0
    in case e of
         V.EvKey (V.KChar 'x') [] -> x %= valid . (+ 0.05)
         V.EvKey (V.KChar 'q') [] -> M.halt
         _ -> return ()
appEvent _ = return ()

initialState :: MyAppState ()
initialState = MyAppState 0.0

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (theBaseAttr,               bg V.brightBlack)
         , (xDoneAttr,                 V.black `on` V.white)
         , (xToDoAttr,                 V.white `on` V.black)
         , (P.progressIncompleteAttr,  fg V.yellow)
         ]

theApp :: M.App (MyAppState ()) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }