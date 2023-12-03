module Attacker(
    runAttacker
)
where

import Network.HTTP.Conduit
import Pacer
import Control.Concurrent
import Data.Time
import Targeter
import GHC.IO.Handle.FD
import GHC.IO.IOMode
import Control.Monad
import GHC.IO.Handle.Text
import Network.HTTP.Types
import Control.Concurrent.Async

writeResultsToFile :: FilePath -> [(Int, NominalDiffTime, Int)] -> IO ()
writeResultsToFile filePath results = do
    withFile filePath WriteMode $ \handle -> do
        forM_ results $ \(hitCount, latency, status) ->
            hPutStrLn handle $ "Hit: " ++ show hitCount ++ ", Latency: " ++ show latency ++ ", Status Code: " ++ show status

attacker :: Manager -> IO (NominalDiffTime, Int)
attacker manager = do
    print "Attacking\n"
    let target = Target "GET" "http://localhost:8000/slow" Nothing []
    requestObj <- request target
    begin <- getCurrentTime
    response <- httpLbs requestObj manager
    end <- getCurrentTime
    let latency = diffUTCTime end begin
    return (latency, statusCode $ responseStatus response)

runAttacker :: PaceConfig -> FilePath -> IO () 
runAttacker config filePath = do
    began <- getCurrentTime
    manager <- newManager tlsManagerSettings
    resultList <- newMVar []

    let loop hitCount = do
            res <- pace began hitCount config
            let PacerResult stop waitTime = res
            if stop 
                then do
                    results <- takeMVar resultList
                    print results
                    writeResultsToFile filePath results
               else do
                    if waitTime > 0
                        then do
                            print $ "Waiting for " ++ show waitTime ++ " seconds"
                            threadDelay (floor $ waitTime * 1000000)
                        else return ()
                    _ <- async $ do
                        (latency, status) <- attacker manager 
                        print("HitCount" ++ show hitCount ++  "Latency: " ++ show latency ++ ", Status Code: " ++ show status)
                        results <- takeMVar resultList
                        putMVar resultList $ results ++ [(hitCount, latency, status)]
                    loop (hitCount + 1)
    loop 0


