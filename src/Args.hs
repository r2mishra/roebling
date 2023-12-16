{-# LANGUAGE OverloadedStrings #-}

module Args where

import Data.Map as Map
import Data.Text (Text)
import Network.URI (URI, parseURI)
import Options.Applicative

data Flags = Flags
  { rate :: Int,
    duration :: Int,
    method :: Text,
    body :: Maybe Text,
    bodyFile :: Maybe FilePath,
    --- Dummy Args simply borrowed from Ali. ---
    headers :: Map String String,
    maxBody :: Int,
    version :: Bool,
    debug :: Bool,
    noKeepAlive :: Bool,
    workers :: Int,
    maxWorkers :: Int,
    connections :: Int,
    noHTTP2 :: Bool,
    localAddress :: Text,
    insecureSkipVerify :: Bool,
    caCert :: FilePath,
    tlsCertFile :: FilePath,
    tlsKeyFile :: FilePath,
    resolvers :: Text,
    queryRange :: Int,
    redrawInterval :: Int,
    timeout :: Int,
    ---------------
    target :: URI -- target url, required arg
  }
  deriving (Show)

flags :: Parser Flags
flags =
  Flags
    <$> option
      auto
      ( long "rate"
          <> short 'r'
          <> help "The request rate per second to issue against the targets. Give 0 then it will send requests as fast as possible."
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "duration"
          <> short 'd'
          <> help "The amount of time to issue requests to the targets. Give 0s for an infinite attack."
          <> value 0
          <> showDefault
      )
    <*> strOption
      ( long "method"
          <> short 'm'
          <> help "An HTTP request method for each request."
          <> value "GET"
          <> showDefault
      )
    <*> optional
      ( strOption
          ( long "body"
              <> short 'b'
              <> help "A request body to be sent."
              <> showDefault
          )
      )
    <*> optional
      ( strOption
          ( long "body-file"
              <> short 'B'
              <> help "The path to file whose content will be set as the http request body."
          )
      )
    <*> option
      auto
      ( long "header"
          <> short 'H'
          <> help "A request header to be sent. Can be used multiple times to send multiple headers."
          <> value (Map.fromList [("User-Agent", "roebling")])
          <> showDefault
      )
    <*> option
      auto
      ( long "max-body"
          <> short 'M'
          <> help "Max bytes to capture from response bodies. Give -1 for no limit."
          <> value 0
          <> showDefault
      )
    <*> switch
      ( long "version"
          <> short 'v'
          <> help "Print the current version."
      )
    <*> switch
      ( long "debug"
          <> help "Run in debug mode."
      )
    <*> switch
      ( long "no-keepalive"
          <> short 'K'
          <> help "Don't use HTTP persistent connection."
      )
    <*> option
      auto
      ( long "workers"
          <> short 'w'
          <> help "Amount of initial workers to spawn."
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "max-workers"
          <> short 'W'
          <> help "Amount of maximum workers to spawn."
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "connections"
          <> short 'c'
          <> help "Amount of maximum open idle connections per target host"
          <> value 0
          <> showDefault
      )
    <*> switch
      ( long "no-http2"
          <> help "Don't issue HTTP/2 requests to servers which support it."
      )
    <*> strOption
      ( long "local-addr"
          <> help "Local IP address."
          <> value "0.0.0.0"
          <> showDefault
      )
    <*> switch
      ( long "insecure"
          <> help "Skip TLS verification"
      )
    <*> strOption
      ( long "cacert"
          <> help "PEM ca certificate file"
          <> value ""
          <> showDefault
      )
    <*> strOption
      ( long "cert"
          <> help "PEM encoded tls certificate file to use"
          <> value ""
          <> showDefault
      )
    <*> strOption
      ( long "key"
          <> help "PEM encoded tls private key file to use"
          <> value ""
          <> showDefault
      )
    <*> strOption
      ( long "resolvers"
          <> help "Custom DNS resolver addresses; comma-separated list."
          <> value ""
          <> showDefault
      )
    <*> option
      auto
      ( long "query-range"
          <> help "The results within the given time range will be drawn on the charts"
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "redraw-interval"
          <> help "Specify how often it redraws the screen"
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "timeout"
          <> short 't'
          <> help "The timeout for each request. 0s means to disable timeouts."
          <> value 0
          <> showDefault
      )
    <*> uriParser

uriParser :: Parser URI
uriParser =
  argument
    (str >>= maybe (fail "Invalid URI") return . parseURI)
    ( metavar "target"
        <> help "The target to attack"
    )
