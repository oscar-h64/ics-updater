module Main where

import Config

import Data.ByteString.Lazy    as B ( writeFile )
import Data.Default            ( Default (def) )
import Data.Either.Combinators ( fromRight' )
import Data.Yaml               ( decodeFileThrow )

import Text.ICalendar          ( parseICalendar, printICalendar )

import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    conf <- decodeFileThrow "config.yaml"

    manager <- newTlsManager
    request <- parseRequest $ confURL conf
    source <- fmap responseBody $ httpLbs request manager

    let decoded = fst $ fromRight' $ parseICalendar def (confURL conf) source

    B.writeFile (confOutpath conf) $ mconcat $ map (printICalendar def) decoded
