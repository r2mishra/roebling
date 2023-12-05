{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ResultLogger(
    runLogger
)
where

import Control.Concurrent
import Attacker (AttackResult(..), AttackResultMessage(..))

-- Dummy file to experiment with channels
runLogger :: Chan AttackResultMessage -> IO ()
runLogger channel = do
    loop Nothing
    where
        loop msg = do
            res <- readChan channel
            case res of
                StopMessage hitCount -> do
                    print $ "Logger ==> Will stop at Hit: " ++ show hitCount
                    loop $ Just (hitCount - 1)
                ResultMessage (AttackResult hitCount code latency) -> do
                    if msg /= Just (hitCount + 1) then  do 
                        print $ "Logger ==> Hit: " ++ show hitCount ++ ", Code: " ++ show code ++ ", Latency: " ++ show latency
                        loop msg
                    else do
                        return ()