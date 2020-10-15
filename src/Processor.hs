--------------------------------------------------------------------------------
-- ICS Updater                                                                --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module Processor (
    processEvents
) where

--------------------------------------------------------------------------------

import Data.Map       as M ( Map, filter )
import Data.Maybe     ( fromMaybe )
import Data.Text.Lazy ( Text, isInfixOf )

import Text.ICalendar ( Date, DateTime, VEvent, descriptionValue, summaryValue,
                        uidValue, veDescription, veSummary, veUID )

import Config         ( Match (..) )

--------------------------------------------------------------------------------

-- | Short type alias for the datatype used to store VEvents in a VCalendar
-- in the iCalendar library
type EventMap = Map (Text, Maybe (Either Date DateTime)) VEvent

-- | `doesMatch` @event match@ checks whether @event@ matches the `Match` value
-- @match@
doesMatch :: VEvent -> Match -> Bool
doesMatch event (UID uid)= uid == uidValue (veUID event)
doesMatch event (Search name) =
    name `isInfixOf` fromMaybe "" (summaryValue <$> veSummary event)
 || name `isInfixOf` fromMaybe "" (descriptionValue <$> veDescription event)

-- | `processEvents` @matchOn events@ removes the events from @events@ which
-- match any of the `Match` values in @matchOn@
processEvents :: [Match] -> EventMap -> EventMap
processEvents matchOn = M.filter $ \event -> not $ any (doesMatch event) matchOn

--------------------------------------------------------------------------------
