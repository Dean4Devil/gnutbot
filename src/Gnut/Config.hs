{-# LANGUAGE TemplateHaskell #-}

module Gnut.Config
    ( parseConfig

    -- Connection Configuration and lenses
    , ConnectionConfig
    , connHost
    , connUser
    , connDomain
    , connPassword

    -- Per-User and lenses
    , UserAccessConfig
    , userRoles
    , userPerms

    -- Channel-specific and lenses
    , ChannelConfig
    , channelAccess

    -- Plugin configuration and lenses
    , PluginConfig
    , load
    , config

    -- Top-level and lenses
    , Config
    , connection
    , roles
    , access
    , channels
    , plugins

    , Roles
    , AccessConfig
    , PerChannelConfig
    , PerPluginConfig
    )
    where

import Gnut.Permission

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.HashMap.Lazy as Map

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))

type Roles = Map.HashMap Text [Permission]

data ConnectionConfig = ConnectionConfig
    { _connHost     :: Text
    , _connUser     :: Text
    , _connDomain   :: Text
    , _connPassword :: Text
    } deriving (Eq, Show)
makeLenses ''ConnectionConfig

data UserAccessConfig = UserAccessConfig
    { _userRoles :: [Text]
    , _userPerms :: [Permission]
    } deriving (Eq, Show)
makeLenses ''UserAccessConfig

type AccessConfig = Map.HashMap Text UserAccessConfig

newtype ChannelConfig = ChannelConfig
    { _channelAccess :: AccessConfig
    } deriving (Eq, Show)
makeLenses ''ChannelConfig

type PerChannelConfig = Map.HashMap Text ChannelConfig

data PluginLoad = Disabled -- Deny loading this Plugin in any channel
                | Inactive -- Don't load this plugin for any channel by default
                | Active   -- Load plugin for every channel by default but allow unloading.
                | Always   -- Force loading. Pluin can not be unloaded.
    deriving (Eq, Show, Ord)

data PluginConfig = PluginConfig
    { _load :: PluginLoad
    , _config :: Map.HashMap Text Y.Value
    } deriving (Eq, Show)
makeLenses ''PluginConfig

type PerPluginConfig = Map.HashMap Text PluginConfig

data Config = Config
    { _connection :: ConnectionConfig
    , _roles :: Roles
    , _access :: AccessConfig
    , _channels :: PerChannelConfig
    , _plugins :: PerPluginConfig
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
        <$> (v .:? "roles" .!= [])
        <*> (v .:? "permissions" .!= [])
    parseJSON _ = error "Bad access config"

instance FromJSON ChannelConfig where
    parseJSON (Y.Object v) = ChannelConfig
        <$> (v .:? "access" .!= Map.empty)
    parseJSON _ = error "Bad channel config"

instance FromJSON PluginLoad where
    parseJSON (Y.String v) = return $ case T.toLower v of
        "always"   -> Always
        "active"   -> Active
        "inactive" -> Inactive
        "disabled" -> Disabled
        _          -> error "Not a valid load setting"
    parseJSON _ = error "load setting must be a string"

instance FromJSON PluginConfig where
    parseJSON (Y.Object v) = PluginConfig
        <$> (v .:? "load" .!= Active)
        <*> (v .:? "config" .!= Map.empty)
    parseJSON _ = error "Bad plugin config"

instance FromJSON Config where
    parseJSON (Y.Object v) = Config
        <$> (v .: "connection")
        <*> (v .:? "roles" .!= Map.empty)
        <*> (v .:? "access" .!= Map.empty)
        <*> (v .:? "channels" .!= Map.empty)
        <*> (v .:? "plugins" .!= Map.empty)
    parseJSON _ = error "Invalid Config"

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile
