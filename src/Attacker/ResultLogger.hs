{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Attacker.ResultLogger
  ( runLogger,
  )
where

import Control.Concurrent
import Utils.Models (AttackResult (..), AttackResultMessage (..))

-- Dummy file to experiment with channels
runLogger :: Chan AttackResultMessage -> IO ()
runLogger channel = do
 loop Nothing 0 
 where
   loop :: Maybe Int -> Int -> IO ()
   loop msg totHits = do
     res <- readChan channel
     case res of
       StopMessage hitCount -> do
         print $ "Logger ==> Will stop when totHits reaches: " ++ show hitCount
         loop (Just (hitCount - 1)) totHits
       ResultMessage (AttackResult hitCount code latency) -> do
         if totHits /= hitCount + 1
           then do
             print $ "Logger ==> Hit: " ++ show hitCount ++ ", Code: " ++ show code ++ ", Latency: " ++ show latency
             loop msg (totHits + 1)
           else do
             return ()

