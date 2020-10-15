--------------------------------------------------------------------------------
-- ICS Updater                                                                --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module Config (
    Match(..),
    Config(..)
) where

--------------------------------------------------------------------------------

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text.Lazy      ( Text, fromStrict )

--------------------------------------------------------------------------------

-- UID: Match the UID of the event exactly
-- Match: Search the Summary and Description fields for the term
data Match = Search Text | UID Text

instance FromJSON Match where
    parseJSON = withObject "Match" $ \v -> case HM.toList v of
        [("uid", String uid)]     -> pure $ UID $ fromStrict uid
        [("search", String term)] -> pure $ Search $ fromStrict term
        x                         -> fail $ "Invalid rule: " ++ show x

--------------------------------------------------------------------------------

-- | Represents the configuration read from the config file
data Config = Config {
    -- | The URL to pull the ICS from
    confURL     :: String,
    -- | The file path to output the updated ICS to
    confOutpath :: String,
    -- | The rules to match on
    confMatchOn :: [Match]
}

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config <$> v .: "sourceURL"
               <*> v .: "outputPath"
               <*> v .: "rules"

--------------------------------------------------------------------------------
