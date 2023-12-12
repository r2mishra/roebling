module Attacker.Attacker
  ( runAttacker,
    AttackResult (..),
    AttackResultMessage (..),
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Time
import Network.HTTP.Conduit
import Network.HTTP.Types
import Attacker.Pacer
import Attacker.Targeter

data AttackResult = AttackResult {seq :: Int, code :: Int, latency :: NominalDiffTime} deriving (Show)

data AttackResultMessage
  = ResultMessage AttackResult
  | StopMessage Int
  deriving (Show)

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
