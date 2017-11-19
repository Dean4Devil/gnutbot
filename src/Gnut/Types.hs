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

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.IO.Class

import Control.Concurrent.MVar
import Control.Lens

import Reactive.Banana.Frameworks

import Network.Xmpp (Session, Message, Jid)

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
