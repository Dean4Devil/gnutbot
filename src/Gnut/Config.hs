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

    -- Top-level and lenses
    , Config
    , connection
    , roles
    , access
    , channels

    , Roles
    , AccessConfig
    , PerChannelConfig
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

data Config = Config
    { _connection :: ConnectionConfig
    , _roles :: Roles
    , _access :: AccessConfig
    , _channels :: PerChannelConfig
    } deriving (Eq, Show)
makeLenses ''Config

instance FromJSON Permission where
    parseJSON (Y.String v) = case T.head v of
        '~' -> do
            let parts = T.splitOn "." (T.tail v)
            return $ Negative (desc parts)
        _   -> do
            let parts = T.splitOn "." v
            return $ Positive (desc parts)
      where desc xs = case last xs of
                "*" -> Wildcard (PermPath (init xs))
                _   -> Precise (PermPath xs)

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

instance FromJSON Config where
    parseJSON (Y.Object v) = Config
        <$> (v .: "connection")
        <*> (v .:? "roles" .!= Map.empty)
        <*> (v .:? "access" .!= Map.empty)
        <*> (v .:? "channels" .!= Map.empty)
    parseJSON _ = error "Invalid Config"

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile
