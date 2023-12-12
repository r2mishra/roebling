module Utils.Models (AttackResult (..), AttackResultMessage (..)) where

import Data.Time

data AttackResult = AttackResult {seq :: Int, code :: Int, latency :: NominalDiffTime, error :: Maybe String} deriving (Show)

data AttackResultMessage
  = ResultMessage AttackResult
  | StopMessage Int
  deriving (Show)
