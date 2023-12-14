-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

module GUI.ProgressBar where

-- import Brick
-- import qualified Brick.AttrMap as A
-- import qualified Brick.Main as M
-- import qualified Brick.Types as T
-- import qualified Brick.Widgets.ProgressBar as P
-- import qualified Graphics.Vty as V
-- import Lens.Micro.Mtl
-- import Lens.Micro.TH

-- data MyAppState n = MyAppState {_x :: Float}

-- makeLenses ''MyAppState

-- drawProgressBar :: MyAppState () -> Widget ()
-- drawProgressBar p = ui
--   where
--     -- use mapAttrNames
--     xBar =
--       updateAttrMap
--         ( A.mapAttrNames
--             [ (xDoneAttr, P.progressCompleteAttr),
--               (xToDoAttr, P.progressIncompleteAttr)
--             ]
--         )
--         $ bar
--         $ _x p
--     lbl c = Just $ show $ fromEnum $ c * 100
--     bar v = P.progressBar (lbl v) v
--     ui = xBar

-- -- TODO: Modify to get input from mediator
-- appEvent :: T.BrickEvent () e -> T.EventM () (MyAppState ()) ()
-- appEvent (T.VtyEvent e) =
--   let valid = clamp (0.0 :: Float) 1.0
--    in case e of
--         V.EvKey (V.KChar 'x') [] -> x %= valid . (+ 0.05)
--         _ -> return ()
-- appEvent _ = return ()

-- updateProgressBar :: Float -> MyAppState ()
-- updateProgressBar a = x %= (clamp (0.0 :: Float) 1.0) . (+ a)

-- initialPBState :: MyAppState ()
-- initialPBState = MyAppState 0.0

-- theBaseAttr :: A.AttrName
-- theBaseAttr = A.attrName "theBase"

-- xDoneAttr, xToDoAttr :: A.AttrName
-- xDoneAttr = theBaseAttr <> A.attrName "X:done"
-- xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

-- theMap :: A.AttrMap
-- theMap =
--   A.attrMap
--     V.defAttr
--     [ (theBaseAttr, bg V.brightBlack),
--       (xDoneAttr, V.green `on` V.green),
--       (xToDoAttr, V.white `on` V.black),
--       (P.progressIncompleteAttr, fg V.black)
--     ]

-- theApp :: M.App (MyAppState ()) e ()
-- theApp =
--   M.App
--     { M.appDraw = drawProgressBar,
--       M.appChooseCursor = M.showFirstCursor,
--       M.appHandleEvent = appEvent,
--       M.appStartEvent = return (),
--       M.appAttrMap = const theMap
--     }
