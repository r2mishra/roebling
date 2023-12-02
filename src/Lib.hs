module Lib
  ( attacker,
  )
where

import Control.Concurrent.Async
import Control.Monad
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Conduit
import Network.HTTP.Types (Status (statusCode))

data AttackResult = AttackResult {code :: Int, latency :: NominalDiffTime} deriving (Show)

callTargetAsync :: String -> IO (Async (Int, NominalDiffTime))
callTargetAsync target = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest target
  let requestWithUA = request {requestHeaders = [("User-Agent", "roebling")]}
  startTime <- getCurrentTime
  async $ do
    response <- httpLbs requestWithUA manager
    let c = statusCode (responseStatus response)
    endTime <- getCurrentTime
    let duration = endTime `diffUTCTime` startTime
    putStrLn $ "Status code: " ++ show c ++ ", latency: " ++ show duration
    return (c, duration)

fetchSameUrlMultipleTimes :: String -> Int -> IO [Async (Int, NominalDiffTime)]
fetchSameUrlMultipleTimes url cnt = do
  replicateM cnt (callTargetAsync url)

numAttacks :: Int
numAttacks = 50

attacker :: String -> IO ()
attacker target = do
  startTime <- getCurrentTime
  putStrLn $ " Start Time: " ++ show startTime
  responses <- fetchSameUrlMultipleTimes target numAttacks
  mapM_ wait responses
  endTime <- getCurrentTime
  putStrLn $ " End Time: " ++ show endTime
