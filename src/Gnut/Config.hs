{-# LANGUAGE TemplateHaskell #-}

module Gnut.Config
    ( parseConfig

    -- Connection Configuration and lenses
    , ConnectionConfig
    , host
    , user
    , domain
    , password

    -- Per-User and lenses
    , UserAccessConfig
    , userPermsFromAccessConfig

    -- Top-level and lenses
    , Config
    , connection
    , access

    , AccessConfig
    )
    where

import Gnut.Permissions
import Control.Lens

import Network.Xmpp.Internal (Jid)

import Data.Maybe

import Network.Xmpp (Jid, jidFromText)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.HashMap.Lazy as Map

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import Data.Aeson (FromJSONKey(..), FromJSONKeyFunction(..))

type Roles = Map.HashMap Text Permissions

data ConnectionConfig = ConnectionConfig
    { _host     :: Text
    , _user     :: Text
    , _domain   :: Text
    , _password :: Text
    } deriving (Eq, Show)
makeLenses ''ConnectionConfig

data UserAccessConfig = UserAccessConfig
    { _permissions :: [Permission]
    } deriving (Eq, Show)
makeLenses ''UserAccessConfig

type AccessConfig = Map.HashMap Text UserAccessConfig

type UserPerms = Map Jid [Permissions]

userPermsFromAccessConfig :: AccessConfig -> UserPerms
userPermsFromAccessConfig a = M.fromList $ map (\(k,v) -> ((fromJust . jidFromText) k, [_permissions v])) $ Map.toList a

data Config = Config
    { _connection :: ConnectionConfig
    , _access :: AccessConfig
    } deriving (Eq, Show)
makeLenses ''Config

instance FromJSON ConnectionConfig where
    parseJSON (Y.Object v) = ConnectionConfig
        <$> (v .: "host")
        <*> (v .: "user")
        <*> (v .: "domain")
        <*> (v .: "password")
    parseJSON _ = error "Bad connection Object"

instance FromJSON UserAccessConfig where
    parseJSON (Y.Object v) = UserAccessConfig
        <$> (v .:? "permissions" .!= [])
    parseJSON _ = error "Bad access config"

instance FromJSON Config where
    parseJSON (Y.Object v) = Config
        <$> (v .: "connection")
        <*> (v .:? "access" .!= Map.empty)
    parseJSON _ = error "Invalid Config"

instance FromJSON Jid where
    parseJSON (Y.String s) = fmap (fromJust . jidFromText) $ pure s
    parseJSON _ = error "Invalid Jid"

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile
