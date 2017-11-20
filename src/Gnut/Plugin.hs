module Gnut.Plugin
    ( 
    )
    where

import Gnut.Types
import Gnut.Xmpp

-- load plugin:
-- load :: PluginConfig -> 
--
-- Plugin loader:
-- map over all plugins:
--      1. decide if plugins needs to be loaded
--      2. if so load plugin with config & load hooks

-- Plugin: Really just a function Message -> Gnut ()
-- Gets every message that Gnut receives. Each Plugin does it's own
-- filtering.
-- (TODO: Later build PluginStores that do better filtering? e.g IMPlugin :: Plugin)
