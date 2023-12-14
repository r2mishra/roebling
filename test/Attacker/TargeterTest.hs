module Attacker.TargeterTest where

import Test.Tasty
import Test.Tasty.HUnit
import Network.HTTP.Types.Header (Header)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Attacker.Targeter (Targeter(..), maybeRequestBody, readFileToString)
import Network.HTTP.Conduit (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyLBS), parseRequest, secure, host, port, path, queryString)
import Utils.Models (Target(..))

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Request should be created correctly" testRequest]


testRequest :: Assertion
testRequest = do
  let target = Target "GET" (fromJust $ parseURI "http://example.com") Nothing Nothing []
  req <- request target
  assertEqual "Method should be GET" (encodeUtf8 "GET") (method req)
  assertEqual "Host should be http://example.com" "\"example.com\"" ( show (host req))
