module Attacker.TargeterTest where

import Attacker.Targeter (Targeter (..), maybeRequestBody, readFileToString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyLBS), host, parseRequest, path, port, queryString, secure)
import Network.HTTP.Types.Header (Header)
import Network.URI (parseURI)
import Test.Tasty
import Test.Tasty.HUnit
import Utils.Models (Target (..))

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [testCase "Request should be created correctly" testRequest]

testRequest :: Assertion
testRequest = do
  let target = Target "GET" (fromJust $ parseURI "http://example.com") Nothing Nothing []
  req <- request target
  assertEqual "Method should be GET" (encodeUtf8 "GET") (method req)
  assertEqual "Host should be http://example.com" "\"example.com\"" (show (host req))
