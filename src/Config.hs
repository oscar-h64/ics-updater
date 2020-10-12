module Config (
    Config(..)
) where

import Data.Aeson
import Data.Text  ( Text )

data Match = Name Text | UID Text

data Config = Config {
    confURL     :: String,
    confOutpath :: String
}

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config <$> v .: "sourceURL"
               <*> v .: "outputPath"
