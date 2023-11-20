module Main (main) where

import Args
import Options.Applicative

main :: IO ()
main = do
    flags <- execParser (info (helper <*> flags) fullDesc)
    print flags
    putStrLn $ "Attacking " ++ show (target flags)