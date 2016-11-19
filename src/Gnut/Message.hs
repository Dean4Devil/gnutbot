module Gnut.Message
    ( Prefix (..)
    , Message (..)
    , makeMessage
    , (==?)
    , (<>)
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char (toLower)

data Prefix
    = ServerPrefix Text
    | NickPrefix Text (Maybe Text) (Maybe Text)
    deriving (Eq, Show)

data Message = Message
    { messagePrefix     :: Maybe Prefix
    , messageCommand    :: Text
    , messageParameters :: [Text]
    } deriving (Eq, Show)

makeMessage :: Text -> [Text] -> Message
makeMessage = Message Nothing

(==?) :: Text -> Text -> Bool
s1 ==? s2 = toLower s1 == toLower s2

toLower :: Text -> Text
toLower = T.map toLower'
  where -- RFC 2812
    toLower' '['  = '{'
    toLower' ']'  = '}'
    toLower' '\\' = '|'
    toLower' '~'  = '^'
    toLower' x = Char.toLower x

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
