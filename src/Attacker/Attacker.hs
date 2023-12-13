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

attacker :: Request -> Manager -> Int -> IO AttackResult
attacker requestObj manager hitCount = do
  begin <- getCurrentTime
  response <- httpLbs requestObj manager
  end <- getCurrentTime
  let status = statusCode $ responseStatus response
      errorMessage = if status /= 200 then Just (getErrorMsg response) else Nothing
   in return (AttackResult hitCount status (end `diffUTCTime` begin) errorMessage)

getErrorMsg :: Response body -> String
getErrorMsg response = show (statusMessage (responseStatus response))

runAttacker :: Chan AttackResultMessage -> Target -> PaceConfig -> IO ()
runAttacker channel target config = do
  began <- getCurrentTime
  manager <- newManager tlsManagerSettings
  targetRequest <- request target

  let loop hitCount = do
        res <- pace began hitCount config
        let PacerResult shouldStop shouldWaitTime = res
        if shouldStop
          then do
            return ()
          else do
            when (shouldWaitTime > 0) $ do
              threadDelay (floor $ shouldWaitTime * 1000000)
            _ <- async $ do
              msg <- attacker targetRequest manager hitCount
              writeChan channel $ ResultMessage msg
            loop (hitCount + 1)
  loop 0
