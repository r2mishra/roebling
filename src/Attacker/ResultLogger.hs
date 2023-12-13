{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Attacker.ResultLogger
  ( runLogger,
  )
where

import Control.Concurrent
import qualified Control.Monad
import Data.Maybe
import System.Timeout
import Utils.Models (AttackResult (..), AttackResultMessage (..))

-- Dummy file to experiment with channels
runLogger :: Chan AttackResultMessage -> IO ()
runLogger channel = loop
  where
    loop = do
      maybeMsg <- timeout (5 * 1000000) (readChan channel) -- 5 seconds timeout (microseconds)
      Control.Monad.when (isJust maybeMsg) $ do
        let ResultMessage (AttackResult hitCount code latency error) = fromJust maybeMsg
        print $ "Logger ==> Hit: " ++ show hitCount ++ ", Code: " ++ show code ++ ", Latency: " ++ show latency ++ ", Error: " ++ show error
        loop
