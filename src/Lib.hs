module Lib
    ( attacker
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types (Status(statusCode))
import System.TimeIt

import Control.Parallel.Strategies
import Control.Monad
import Control.Concurrent.Async
import Data.ByteString.Lazy
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)

data AttackResult = AttackResult { code :: Int, latency :: NominalDiffTime } deriving (Show)


-- wait for respoinse
attack :: String -> IO (AttackResult)
attack target = do
    asyncResponse <- callTargetAsync target
    (c, duration) <- wait asyncResponse
    return (AttackResult c duration)

callTarget :: String -> IO Int
callTarget target = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest target
    let requestWithUA = request { requestHeaders = [("User-Agent", "roebling")] }
    response <- httpLbs requestWithUA manager
    let c = statusCode (responseStatus response)
    return c

-- Async callTarget
callTargetAsync :: String -> IO (Async (Int, NominalDiffTime))
callTargetAsync target = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest target
    let requestWithUA = request { requestHeaders = [("User-Agent", "roebling")] }
    startTime <- getCurrentTime
    async $ do
        response <- httpLbs requestWithUA manager
        let c = statusCode (responseStatus response)
        endTime <- getCurrentTime
        let duration = endTime `diffUTCTime` startTime
        return (c, duration)

fetchSameUrlMultipleTimes :: String -> Int -> IO [AttackResult]
fetchSameUrlMultipleTimes url count = do
    replicateM count (attack url)


printAttackResult :: AttackResult -> IO ()
printAttackResult (AttackResult c d) = putStrLn $ "Status code: " ++ show c ++ ", latency: " ++ show d

numAttacks::Int
numAttacks = 100

attacker :: String -> IO ()
attacker target = do
    responses <- fetchSameUrlMultipleTimes target numAttacks
    mapM_  printAttackResult responses
