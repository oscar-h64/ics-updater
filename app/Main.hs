module Main where

import Data.ByteString.Lazy    as B ( writeFile )
import Data.Default            ( Default (def) )
import Data.Either.Combinators ( fromRight' )
import Data.Yaml               ( decodeFileThrow )

import Text.ICalendar

import Network.HTTP.Client
import Network.HTTP.Client.TLS ( newTlsManager )

import System.Environment

import Config                  ( Config (..) )
import Processor

main :: IO ()
main = do
    [confPath] <- getArgs

    Config{..} <- decodeFileThrow confPath
    manager <- newTlsManager
    request <- parseRequest confURL
    source <- fmap responseBody $ httpLbs request manager

    let decoded = head $ fst $ fromRight' $ parseICalendar def confURL source

    let updated = decoded{vcEvents = processEvents confMatchOn $ vcEvents decoded}

    B.writeFile confOutpath $ printICalendar def updated
