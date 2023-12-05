module Main (main) where

import qualified Args
import Attacker (runAttacker)
import Control.Concurrent (newChan)
import Control.Concurrent.Async
import Data.Text (unpack)
import Options.Applicative
import qualified Pacer
import ResultLogger (runLogger)
import Targeter (Target (..))

main :: IO ()
main = do
  cmdFlags <- execParser (info (helper <*> Args.flags) fullDesc)
  let targetter = buildTargetter cmdFlags
  let pacer = buildPacer cmdFlags
  attackChannel <- newChan
  attackerThread <- async $ runAttacker attackChannel targetter pacer
  fetcherThread <- async $ runLogger attackChannel
  wait attackerThread
  wait fetcherThread

buildTargetter :: Args.Flags -> Target
buildTargetter cmdFlags =
  Target
    { url = unpack (Args.target cmdFlags),
      verb = Args.method cmdFlags,
      body = Just [Args.body cmdFlags],
      headers = [("Content-Type", "application/json")]
    }

buildPacer :: Args.Flags -> Pacer.PaceConfig
buildPacer cmdFlags =
  Pacer.PaceConfig
    { Pacer.rate = Args.rate cmdFlags,
      Pacer.duration = fromIntegral (Args.duration cmdFlags)
    }
