module Gnut.Message.Encoder
    ( encodePrefix
    , encode
    ) where

import           Control.Applicative    ((<$>), (<|>))
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromMaybe, isJust)
import           Data.Monoid           (Monoid, mappend)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import           Gnut.Message

encodePrefix :: Prefix -> B.ByteString
encodePrefix (ServerPrefix server) = T.encodeUtf8 server
encodePrefix (NickPrefix nick user host) =
    ":" <> T.encodeUtf8 nick
    <> fromMaybe "" (fmap (("!" <>) . T.encodeUtf8) user)
    <> fromMaybe "" (fmap (("@" <>) . T.encodeUtf8) host)

encodeCommand :: Text -> B.ByteString
encodeCommand = T.encodeUtf8

encodeParameters :: [Text] -> B.ByteString
encodeParameters [] = mempty
encodeParameters [x]
    | hasSpace x || T.null x || T.head x == ':' = " :" <> T.encodeUtf8 x
    | otherwise                              = " "  <> T.encodeUtf8 x
  where hasSpace = isJust . T.find (== ' ')
encodeParameters (x : xs) = " " <> T.encodeUtf8 x <> encodeParameters xs

encode :: Message -> B.ByteString
encode (Message pre cmd param) = 
    encodePrefix' pre <> encodeCommand cmd <> encodeParameters param
  where encodePrefix' = fromMaybe mempty . fmap ((<> " ") . encodePrefix)
