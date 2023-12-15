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
runLogger :: String -> Chan AttackResultMessage -> IO ()
runLogger fn channel = loop
  where
    loop = do
      maybeMsg <- timeout (5 * 1000000) (readChan channel) -- 5 seconds timeout (microseconds)
      Control.Monad.when (isJust maybeMsg) $ do
        let ResultMessage (AttackResult hitCount code latency error bytesIn bytesOut) = fromJust maybeMsg
        appendFile fn ("Logger ==> Hit: " ++ show hitCount ++ ", Code: " ++ show code ++ ", Latency: " ++ show latency ++ ", Error: " ++ show error ++ ", Bytes In: " ++ show bytesIn ++ ", Bytes Out: " ++ show bytesOut ++ "\n")
        loop
