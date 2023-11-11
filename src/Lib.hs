{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( attacker
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types (Status(statusCode))
import System.TimeIt

data AttackResult = AttackResult { code :: Int, latency :: Double } deriving (Show)

endpoint :: String
endpoint = "http://localhost:8000/slow"

attack :: IO AttackResult
attack = do
    (duration, c) <- timeItT callTarget
    return $ AttackResult c duration

callTarget :: IO Int
callTarget = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest endpoint
    let requestWithUA = request { requestHeaders = [("User-Agent", "roebling")] }
    response <- httpLbs requestWithUA manager
    let c = statusCode (responseStatus response)
    return c

printAttackResult :: AttackResult -> IO ()
printAttackResult (AttackResult c d) = putStrLn $ "Status code: " ++ show c ++ ", latency: " ++ show d

attacker :: IO ()
attacker = printAttackResult =<< attack


