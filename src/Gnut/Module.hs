{-# LANGUAGE QuasiQuotes #-}
module Gnut.Module where

import Prelude hiding (lookup)

import Network.Xmpp.Internal

import Data.Map (Map)
import qualified Data.Map as M

import Reactive.Banana
import Reactive.Banana.Frameworks

import Gnut.Types
import Gnut.Modules.Hello


type ModuleAction = (Stanza -> IO ())
type Modules = Map Jid ModuleAction
type ModuleStore = Behavior Modules
type ModuleHandler = Handler Modules

findModule :: Stanza -> Jid
findModule s = [jid|hello@gnut|]

startModules = M.fromList [ ([jid|hello@gnut|], handleStanza Hello) ]

lookup :: Modules -> Jid -> Maybe ModuleAction
lookup m j = M.lookup j m

lookupS :: Modules -> Stanza -> Maybe ModuleAction
lookupS m s = lookup m $ findModule s

applyMod :: Modules -> Stanza -> Maybe (IO ())
applyMod m s = do
    mod <- lookupS m s
    return $ mod s

keys = M.keys

setupModuleStore :: ModuleHandler -> (AddHandler Stanza) -> IO ()
setupModuleStore h input = do
    nw <- setupModuleNetwork h input
    actuate nw
    return ()

setupModuleNetwork :: ModuleHandler -> (AddHandler Stanza) -> IO (EventNetwork)
setupModuleNetwork h input = compile $ do
    emsg <- fromAddHandler input

    return ()
