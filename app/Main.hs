--------------------------------------------------------------------------------
-- ICS Updater                                                                --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Data.ByteString.Lazy    as B ( writeFile )
import Data.Default            ( Default (def) )
import Data.Either.Combinators ( fromRight' )
import Data.Yaml               ( decodeFileThrow )

import Text.ICalendar

import Network.HTTP.Client
import Network.HTTP.Client.TLS ( newTlsManager )

import System.Environment

import Config                  ( Config (..) )
import Processor               ( processEvents )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- get path for config file - this should be the only argument
    [confPath] <- getArgs

    -- get the configuration from the file specified
    Config{..} <- decodeFileThrow confPath

    -- download the source ICS file into a bytestring
    manager <- newTlsManager
    request <- parseRequest confURL
    source <- fmap responseBody $ httpLbs request manager

    -- decode this into a list of VCalendars. This ignores all warnings, and
    -- will crash on error as it cannot continue if ICS cannot be decoded
    let decoded = fst $ fromRight' $ parseICalendar def confURL source

    -- filter the events of every calendar using `processEvents`
    let updateCal c = c{vcEvents = processEvents confMatchOn $ vcEvents c}
    let updated = map updateCal decoded

    -- convert each VCalendar to a bytestring, and combine them to make the
    -- whole ICS
    B.writeFile confOutpath $ mconcat $ map (printICalendar def) updated

--------------------------------------------------------------------------------
