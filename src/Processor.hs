module Processor (
    processEvents
) where

import Data.Map       as M ( Map, filter )
import Data.Maybe     ( fromMaybe )
import Data.Text.Lazy ( Text, isInfixOf )

import Text.ICalendar ( Date, DateTime, VEvent, descriptionValue, summaryValue,
                        uidValue, veDescription, veSummary, veUID )

import Config         ( Match (..) )

type EventMap = Map (Text, Maybe (Either Date DateTime)) VEvent

doesMatch :: VEvent -> Match -> Bool
doesMatch event (UID uid)= uid == uidValue (veUID event)
doesMatch event (Search name) = name `isInfixOf` fromMaybe "" (summaryValue <$> veSummary event)
                             || name `isInfixOf` fromMaybe "" (descriptionValue <$> veDescription event)

processEvents :: [Match] -> EventMap -> EventMap
processEvents matchOn = M.filter $ \event -> not $ any (doesMatch event) matchOn
