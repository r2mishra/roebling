{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Targeter where

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Conduit
import Network.HTTP.Simple (RequestHeaders)

data Target = Target
  { verb :: ByteString,
    url :: String,
    body :: Maybe [Text],
    header :: RequestHeaders
  }
class Targeter a where
    request :: a -> IO Request

instance Targeter Target where
    request :: Target -> IO Request
    request target = do
        let requestMethod = verb target
            requestUrl = url target
            reqBody = maybeRequestBody (body target)
            reqHeaders = header target
        reqBuilder <- parseRequest requestUrl
        return reqBuilder {method = requestMethod, requestBody = reqBody, requestHeaders = reqHeaders}

maybeRequestBody :: Maybe [Text] -> RequestBody
maybeRequestBody Nothing = RequestBodyLBS ""
maybeRequestBody (Just bodyText) = setRequestBodyJSON bodyText

setRequestBodyJSON :: (ToJSON a) => a -> RequestBody
setRequestBodyJSON = RequestBodyLBS . encode
