{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Attacker.Targeter where

import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyLBS), parseRequest)
import Network.HTTP.Simple (RequestHeaders)

data Target = Target
  { verb :: Text,
    url :: String,
    body :: Maybe [Text],
    headers :: RequestHeaders
  }

class Targeter a where
  request :: a -> IO Request

instance Targeter Target where
  request :: Target -> IO Request
  request target = do
    let requestMethod = verb target
        requestUrl = url target
        reqBody = maybeRequestBody (body target)
        reqHeaders = headers target
    reqBuilder <- parseRequest requestUrl
    return reqBuilder {method = encodeUtf8 requestMethod, requestBody = reqBody, requestHeaders = reqHeaders}

maybeRequestBody :: Maybe [Text] -> RequestBody
maybeRequestBody Nothing = RequestBodyLBS ""
maybeRequestBody (Just bodyText) = setRequestBodyJSON bodyText

setRequestBodyJSON :: (ToJSON a) => a -> RequestBody
setRequestBodyJSON = RequestBodyLBS . encode
