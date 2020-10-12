module Main where

import Data.ByteString.Lazy    as B ( writeFile )
import Data.Default            ( Default (def) )
import Data.Either.Combinators ( fromRight' )
import Data.Yaml               ( decodeFileThrow )

import Text.ICalendar

import Network.HTTP.Client
import Network.HTTP.Client.TLS ( newTlsManager )

import Config                  ( Config (..) )
import Processor

main :: IO ()
main = do
    Config url outpath matchOn <- decodeFileThrow "config.yaml"

    manager <- newTlsManager
    request <- parseRequest url
    source <- fmap responseBody $ httpLbs request manager

    let decoded = head $ fst $ fromRight' $ parseICalendar def url source

    let updated = decoded{vcEvents = processEvents matchOn $ vcEvents decoded}

    B.writeFile outpath $ printICalendar def updated
