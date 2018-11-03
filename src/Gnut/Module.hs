{-# LANGUAGE QuasiQuotes #-}
module Gnut.Module where

import Prelude hiding (lookup)

import Network.Xmpp.Internal

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe

import Reactive.Banana
import Reactive.Banana.Frameworks

import Gnut.Types
import Gnut.Modules.Hello


type ModuleStore = Behavior [(ModuleFilter, ModuleHandler)]
type ModuleFilter = (Stanza -> Bool)
type ModuleHandler = Handler (Stanza, ReplyCallback)
type ReplyCallback = Handler Stanza
