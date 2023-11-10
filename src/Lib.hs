{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Network.HTTP.Conduit



endpoint :: String
endpoint = "http://localhost:8000/slow"


callGithub :: IO ()
callGithub = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest endpoint
    let requestWithUA = request { requestHeaders = [("User-Agent", "haskell")] }
    response <- httpLbs requestWithUA manager
    putStrLn $ "The status code was: " ++ show (responseStatus response)


someFunc :: IO ()
someFunc = callGithub


