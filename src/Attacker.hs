module Attacker(
    runAttacker,
    AttackResult(..),
    AttackResultMessage(..),
)
where

import Network.HTTP.Conduit
import Pacer
import Control.Concurrent
import Data.Time
import Targeter
import Control.Monad
import Network.HTTP.Types
import Control.Concurrent.Async

data AttackResult = AttackResult {seq:: Int, code :: Int, latency :: NominalDiffTime} deriving (Show)
data AttackResultMessage
  = ResultMessage AttackResult
  | StopMessage Int
  deriving Show

attacker :: Manager -> Int -> IO (NominalDiffTime, Int)
attacker manager hit = do
    let parameterized_url = "http://localhost:8000/slow/" ++ show hit
    let target = Target "GET" parameterized_url Nothing []
    requestObj <- request target
    begin <- getCurrentTime
    response <- httpLbs requestObj manager
    end <- getCurrentTime
    return (end `diffUTCTime` begin, statusCode $ responseStatus response)

runAttacker :: Chan AttackResultMessage -> PaceConfig-> IO ()
runAttacker channel config = do
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
                        (atkLatency, status) <- attacker manager hitCount
                        writeChan channel $ ResultMessage $ AttackResult hitCount status atkLatency
                    loop (hitCount + 1)
    loop 0


