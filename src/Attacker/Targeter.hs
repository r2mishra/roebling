{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Attacker.Targeter where

import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Network.HTTP.Conduit (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyLBS), parseRequest)
import Utils.Models (Target (body, bodyFile, headers, url, verb))

class Targeter a where
  request :: a -> IO Request

instance Targeter Target where
  request :: Target -> IO Request
  request target = do
    let requestMethod = verb target
        requestUrl = url target
        reqHeaders = headers target
    reqBody <- maybeRequestBody (body target) (bodyFile target)
    reqBuilder <- parseRequest (show requestUrl)
    return reqBuilder {method = encodeUtf8 requestMethod, requestBody = reqBody, requestHeaders = reqHeaders}

maybeRequestBody :: Maybe Text -> Maybe FilePath -> IO RequestBody
maybeRequestBody Nothing Nothing = return (RequestBodyLBS "")
maybeRequestBody (Just bodyText) _ = return (setRequestBodyJSON bodyText)
maybeRequestBody Nothing (Just filePath) = do
  bodyText <- readFileToString filePath
  return (setRequestBodyJSON bodyText)

setRequestBodyJSON :: (ToJSON a) => a -> RequestBody
setRequestBodyJSON = RequestBodyLBS . encode

readFileToString :: FilePath -> IO String
readFileToString filePath = do
  content <- TIO.readFile filePath
  return (show content)
