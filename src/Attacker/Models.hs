module Attacker.Models (AttackResult, AttackResultMessage) where

import Data.Time

data AttackResult = AttackResult {seq :: Int, code :: Int, latency :: NominalDiffTime} deriving (Show)

data AttackResultMessage
  = ResultMessage AttackResult
  | StopMessage Int
  deriving (Show)
