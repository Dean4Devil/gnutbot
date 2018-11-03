module Gnut.Router
    ( setupRouterNetwork
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

setupRouterNetwork :: AddHandler Stanza -> Handler Stanza -> ModuleStore -> Map Jid Permissions -> IO EventNetwork
setupRouterNetwork esin hout bplugins uperm = compile $ do
    -- Stanzas the XMPP module has forwarded to us specifically
    ein <- fromAddHandler esin

    -- TODO Figure this valueB out like in Xmpp.hs
    plugins <- valueB bplugins
    let
        ea = fmap (\s -> (getPlugins plugins s, s)) ein
        ea :: Event ([ModuleHandler], Stanza)

        eb = fmap (\(m, s) -> (m, (s, [permLookup s]))) ea
        eb :: Event ([ModuleHandler], (Stanza, [Permissions]))

        ec = fmap (\(m, s) -> (m, (s, hout))) eb
        ec :: Event ([ModuleHandler], ((Stanza, [Permissions]), ReplyCallback))

        ed = fmap (\(m, x) -> map (\f -> f x) m) ec
        ed :: Event ([IO ()])

        eend = fmap (mapM_ id) ed
        eend :: Event (IO ())

    reactimate eend
    reactimate $ fmap (print . snd) eb
  where
    permLookup s = permLookup' uperm s

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
  where
    extractJid (IQRequestS i) = iqRequestFrom i
    extractJid (IQResultS i) = iqResultFrom i
    extractJid (IQErrorS i) = iqErrorFrom i
    extractJid (MessageS m) = messageFrom m
    extractJid (MessageErrorS m) = messageErrorFrom m
    extractJid (PresenceS p) = presenceFrom p
    extractJid (PresenceErrorS p) = presenceErrorFrom p
