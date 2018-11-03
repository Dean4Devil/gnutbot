{-# LANGUAGE QuasiQuotes #-}
module Gnut.Module where

import Prelude hiding (lookup)

import Network.Xmpp.Internal hiding (Plugin)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe

import Reactive.Banana
import Reactive.Banana.Frameworks

import Gnut.Types
import Gnut.Permissions

import qualified Gnut.Modules.Hello as Hello
import qualified Gnut.Modules.Echo as Echo


type Module = (ModuleFilter, ModuleHandler)
type ModuleStore = Behavior [Module]
type ModuleUpdater = Handler [Module]
type ModuleFilter = (Stanza -> Bool)
type ModuleHandler = Handler ((Stanza, [Permissions]), ReplyCallback)
type ReplyCallback = Handler Stanza


setupModuleNetwork :: ModuleUpdater -> IO EventNetwork
setupModuleNetwork update = compile $ do

    return ()

pureModuleStore :: ModuleStore
pureModuleStore = pure $ [storePlugin Hello.getPlugin, storePlugin Echo.getPlugin]


storePlugin :: Plugin -> (ModuleFilter, ModuleHandler)
storePlugin (Plugin _ f a) = (f, uncurry . uncurry $ a)
