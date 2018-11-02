module Gnut.Router
    ( setupRouterNetwork
    )
    where

import Prelude hiding (lookup)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Network.Xmpp.Internal

import Gnut.Types
import Gnut.Module
import Gnut.Permissions

-- Routers job:
-- 1. Filter all stanzas that are
--      - invalid
--      - sent by ignored sources (jids, mucs, external people, ..)
-- 2. Find the Module(s) that want to handle that stanza
--      - Rewrite the JID from gnut@paranoidlabs.org to <module>@gnut
--      - How to treat exclusivity (e.g. permission-, log-, module-store- module)?
-- 3. Send the stanza to that source(s)

-- Returns Just s ∀ valid s, Nothing otherwise
filterInvalid' :: Stanza -> Maybe Stanza
filterInvalid' = Just

filterInvalid s = filterJust $ fmap filterInvalid' s

filterIgnored = filterNoPerm "gnut.ignore"

setupRouterNetwork :: PermBehavior -> (AddHandler Stanza) -> IO EventNetwork
setupRouterNetwork ignored esmsg = compile $ do
    (mods,h) <- newBehavior startModules

    liftIOLater $ setupModuleStore h esmsg

    emsg <- fromAddHandler esmsg

    let
        -- lookup module to run -> Event (mod)
        --
        modules = fmap applyMod mods
        efiltered = filterInvalid emsg
        eunignore = filterIgnored efiltered
        out' = apply modules emsg
        out = filterJust out'

        curmods = mods <@ emsg

    reactimate $ fmap id out
