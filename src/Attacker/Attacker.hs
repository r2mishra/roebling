module Attacker.Attacker
  ( runAttacker,
  )
where

import Attacker.Pacer
import Attacker.Targeter
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Time
import Network.HTTP.Conduit
import Network.HTTP.Types
import Utils.Models

attacker :: Target -> Manager -> IO (NominalDiffTime, Int)
attacker target manager = do
  requestObj <- request target
  begin <- getCurrentTime
  response <- httpLbs requestObj manager
  end <- getCurrentTime
  return (end `diffUTCTime` begin, statusCode $ responseStatus response)

runAttacker :: Chan AttackResultMessage -> Target -> PaceConfig -> IO ()
runAttacker channel target config = do
  began <- getCurrentTime
  manager <- newManager tlsManagerSettings

  let loop hitCount = do
        res <- pace began hitCount config
        let PacerResult shouldStop shouldWaitTime = res
        if shouldStop
          then do
            writeChan channel $ StopMessage hitCount
            return ()
          else do
            when (shouldWaitTime > 0) $ do
              threadDelay (floor $ shouldWaitTime * 1000000)
            _ <- async $ do
              (atkLatency, status) <- attacker target manager
              writeChan channel $ ResultMessage $ AttackResult hitCount status atkLatency
            loop (hitCount + 1)
  loop 0