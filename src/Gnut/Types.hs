{-# LANGUAGE TemplateHaskell #-}
module Gnut.Types
    ( Gnut

    , GnutState(..)
    , session
    , config

    , PluginContext
    , globalconfig
    , loaded
    , userjidmap

    , runGnut
    , get
    , put
    )
    where

import Gnut.Config hiding (config)
import Gnut.Permission
import Gnut.Xmpp

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.IO.Class

import Control.Concurrent.MVar
import Control.Lens

import Reactive.Banana.Frameworks

import qualified Data.HashMap.Lazy as Map

import Data.Text (Text)

type Gnut a = StateT GnutState a

runGnut = runStateT

data GnutState = GnutState
    { _session :: Session
    , _config :: MVar Config
    }
makeLenses ''GnutState

type Plugin = Message -> IO ()

data PluginContext = PluginContext
    { _globalconfig :: MVar Config
    , _loaded       :: Map.HashMap Text Plugin
    , _userjidmap   :: Map.HashMap Text Jid
    }
makeLenses ''PluginContext

{-
 -data AuthContext = AuthContext
 -    { 
 -    }
 -
 -type Roles = Map.HashMap Text [Permission]
 -
 -data AuthObject = AuthObject
 -    { _globalRoles :: Roles
 -    , _globalPerms :: [Permission]
 -    , _contextRoles :: Context -> Roles
 -    , _contextPerms :: Context -> [Permission]
 -    }
 -makeLenses ''AuthObject
 -
 -type AuthUser = Map.HashMap Jid AuthObject
 -}
