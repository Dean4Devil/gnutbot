module Gnut.Config
    ( parseConfig
    , Config(..)
    )
    where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Network.Socket


data Config = Config
    { cHost :: Text
    , cUser :: Text
    , cDomain :: Text
    , cPW :: Text
    }
    deriving (Eq, Show)
instance FromJSON Config where
    parseJSON (Y.Object v) =
        Config <$>
        v .: "host" <*>
        v .: "user" <*>
        v .: "domain" <*>
        v .: "password"
    parseJSON _ = fail "Expected object"


parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile
