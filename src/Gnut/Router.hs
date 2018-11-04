module Gnut.Router
    ( setupRouterNetwork
    , mangleMuc
    )
    where

import Prelude hiding (lookup)

import Control.Monad

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Map (Map, lookup)

import Network.Xmpp.Internal

import Gnut.Types
import Gnut.Module
import Gnut.Permissions

setupRouterNetwork :: (Stanza -> Stanza) -> AddHandler Stanza -> Handler Stanza -> ModuleStore -> Map Jid Permissions -> IO EventNetwork
setupRouterNetwork mngl esin hout bplugins uperm = compile $ do
    -- Stanzas the XMPP module has forwarded to us specifically
    ein <- fromAddHandler esin

    let
        ba = fmap (\p s -> (getPlugins p s, s)) bplugins
        ba :: Behavior (Stanza -> ([ModuleHandler], Stanza))

        ea = ba <@> ein
        ea :: Event ([ModuleHandler], Stanza)

        eb = fmap (\(m, s) -> (m, (s, [permLookup s]))) ea
        eb :: Event ([ModuleHandler], (Stanza, [Permissions]))

        ec = fmap (\(m, s) -> (m, (s, (hout . mngl)))) eb
        ec :: Event ([ModuleHandler], ((Stanza, [Permissions]), ReplyCallback))

        ed = fmap (\(m, x) -> map (\f -> f x) m) ec
        ed :: Event ([IO ()])

        eend = fmap (mapM_ id) ed
        eend :: Event (IO ())

    reactimate eend
  where
    permLookup s = permLookup' uperm s

mangleMuc :: Stanza -> Stanza
mangleMuc (MessageS m) = MessageS $ m { messageType = GroupChat
                                      , messageTo = fmap toBare (messageTo m)
                                      }

getPlugins :: [(ModuleFilter, ModuleHandler)] -> Stanza -> [ModuleHandler]
getPlugins p s = map snd $ filter (\(f,h) -> f s) p

permLookup' :: Map Jid Permissions -> Stanza -> Permissions
permLookup' m s = case lookupS m s of
    Just p -> p
    Nothing -> []

lookupS :: Map Jid Permissions -> Stanza -> Maybe Permissions
lookupS m s = do
    j <- extractJid s
    lookup (toBare j) m
