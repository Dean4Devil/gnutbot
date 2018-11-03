module Gnut.Router
    ( setupRouterNetwork
    )
    where

import Prelude hiding (lookup)

import Control.Monad

import Reactive.Banana
import Reactive.Banana.Frameworks

import Network.Xmpp.Internal

import Gnut.Types
import Gnut.Module
import Gnut.Permissions

setupRouterNetwork :: AddHandler Stanza -> Handler Stanza -> ModuleStore -> IO EventNetwork
setupRouterNetwork esin hout bplugins = compile $ do
    -- Stanzas the XMPP module has forwarded to us specifically
    ein <- fromAddHandler esin

    -- TODO Figure this valueB out like in Xmpp.hs
    plugins <- valueB bplugins
    let
        ea = fmap (\s -> (getPlugins plugins s, s)) ein
        ea :: Event ([ModuleHandler], Stanza)
        eb = fmap (\(m, s) -> (m, (s, []))) ea
        eb :: Event ([ModuleHandler], (Stanza, [Permissions]))
        ec = fmap (\(m, s) -> (m, (s, hout))) eb
        ec :: Event ([ModuleHandler], ((Stanza, [Permissions]), ReplyCallback))
        ed = fmap (\(m, x) -> map (\f -> f x) m) ec
        ed :: Event ([IO ()])
        eend = fmap (mapM_ id) ed
        eend :: Event (IO ())

    reactimate eend

getPlugins :: [(ModuleFilter, ModuleHandler)] -> Stanza -> [ModuleHandler]
getPlugins p s = map snd $ filter (\(f,h) -> f s) p
