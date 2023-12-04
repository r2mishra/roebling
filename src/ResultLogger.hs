module ResultLogger(
    runLogger
)
where

import Control.Concurrent
import Data.Time
-- Dummy file to experiment with channels

runLogger :: Chan(Maybe(Int, NominalDiffTime)) -> IO ()
runLogger channel = do
    print "Logger running"
    loop 0
    where
        loop i = do
            res <- readChan channel
            case res of
                Just(hitCount, latency) -> do
                    print $ "Logger ==> Hit: " ++ show hitCount ++ ", Latency: " ++ show latency
                    loop (i + 1)
                Nothing -> return ()
