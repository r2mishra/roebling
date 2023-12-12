{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ProgressBar where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types
  ( Widget,
  )
import qualified Brick.Types as T
import Brick.Util (bg, clamp, fg, on)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core
  ( overrideAttr,
    str,
    updateAttrMap,
    (<+>),
    (<=>),
  )
import qualified Brick.Widgets.ProgressBar as P
import Control.Monad
import Control.Monad (void)
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH

data MyAppState n = MyAppState {_x :: Float}

makeLenses ''MyAppState

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
  where
    -- use mapAttrNames
    xBar =
      updateAttrMap
        ( A.mapAttrNames
            [ (xDoneAttr, P.progressCompleteAttr),
              (xToDoAttr, P.progressIncompleteAttr)
            ]
        )
        $ bar
        $ _x p
    lbl c = Just $ show $ fromEnum $ c * 100
    bar v = P.progressBar (lbl v) v
    ui =
      (Brick.str "X: " <+> xBar)
        <=> Brick.str ""
        <=> Brick.str "Hit 'x' to advance progress, or 'q' to quit"

-- TODO: Modify to get input from mediator
appEvent :: T.BrickEvent () e -> T.EventM () (MyAppState ()) ()
appEvent (T.VtyEvent e) =
  let valid = clamp (0.0 :: Float) 1.0
   in case e of
        V.EvKey (V.KChar 'x') [] -> x %= valid . (+ 0.05)
        V.EvKey (V.KChar 'q') [] -> M.halt
        _ -> return ()
appEvent _ = return ()

initialPBState :: MyAppState ()
initialPBState = MyAppState 0.0

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (theBaseAttr, bg V.brightBlack),
      (xDoneAttr, V.black `on` V.white),
      (xToDoAttr, V.white `on` V.black),
      (P.progressIncompleteAttr, fg V.yellow)
    ]

theApp :: M.App (MyAppState ()) e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }
