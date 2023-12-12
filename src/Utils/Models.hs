module Utils.Models (AttackResult (..), AttackResultMessage (..), Target (..), PaceConfig (..), PacerResult (..)) where

import Data.Text
import Data.Time
import Network.HTTP.Simple (RequestHeaders)
import Network.URI (URI)

data AttackResult = AttackResult {seq :: Int, code :: Int, latency :: NominalDiffTime, error :: Maybe String} deriving (Show)

data AttackResultMessage
  = ResultMessage AttackResult
  | StopMessage Int
  deriving (Show)

data Target = Target
  { verb :: Text,
    url :: URI,
    body :: Maybe Text,
    bodyFile :: Maybe FilePath,
    headers :: RequestHeaders
  }

data PaceConfig = PaceConfig {rate :: Int, duration :: NominalDiffTime} deriving (Show)

data PacerResult = PacerResult {stop :: Bool, waitTime :: NominalDiffTime} deriving (Show)
