module Lib
    ( attacker
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types (Status(statusCode))
import System.TimeIt

import Control.Parallel.Strategies
import Control.Monad

data AttackResult = AttackResult { code :: Int, latency :: Double } deriving (Show)

attack :: String -> IO AttackResult
attack target = do
    (duration, c) <- timeItT (callTarget target)
    return (AttackResult c duration)

callTarget :: String -> IO Int
callTarget target = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest target
    let requestWithUA = request { requestHeaders = [("User-Agent", "roebling")] }
    response <- httpLbs requestWithUA manager
    let c = statusCode (responseStatus response)
    return c

printAttackResult :: AttackResult -> IO ()
printAttackResult (AttackResult c d) = putStrLn $ "Status code: " ++ show c ++ ", latency: " ++ show d


numAttacks = 100

attacker :: String -> IO ()
attacker target = do
    attacks <- replicateM numAttacks (attack target >>= printAttackResult)
    return ()