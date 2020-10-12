module Config (
    Match(..),
    Config(..)
) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text.Lazy      ( Text, fromStrict )

-- UID: Match the UID of the event exactly
-- Match: Search the Summary and Description fields for the term
data Match = Search Text | UID Text

instance FromJSON Match where
    parseJSON = withObject "Match" $ \v -> case HM.toList v of
        [("uid", String uid)]     -> pure $ UID $ fromStrict uid
        [("search", String term)] -> pure $ Search $ fromStrict term
        x                         -> fail $ "Invalid rule: " ++ show x

data Config = Config {
    confURL     :: String,
    confOutpath :: String,
    confMatchOn :: [Match]
}

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config <$> v .: "sourceURL"
               <*> v .: "outputPath"
               <*> v .: "rules"
