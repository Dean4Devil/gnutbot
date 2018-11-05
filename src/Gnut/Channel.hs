module Gnut.Channel
    ( setupChannelNetwork
    ) where

import Prelude hiding (lookup)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Network.Xmpp.Internal hiding (Plugin)

import Data.Map hiding (split)

import Gnut.Types
import Gnut.Permissions

setupChannelNetwork :: AddHandler Stanza
                    -> AddHandler PlugUpdate
                    -> (Stanza -> Stanza)
                    -> Handler Stanza
                    -> ChannelSettings
                    -> IO EventNetwork
setupChannelNetwork esinput esplugin mangle hout defaults = compile $ do
    einput <- fromAddHandler esinput
    eplugin <- fromAddHandler esplugin

    (bplugin, hplugin) <- newBehavior $ csPlugins defaults
    (bpermission, hpermission) <- newBehavior $ csPermissions defaults

    let 
        (eplugload, eplugunload) = split eplugin

        -- uplug(un)load is an Event containing the *updated* plugin map.
        uplugload = plugInsert <$> bplugin <@> eplugload
        uplugload :: Event (Map String Plugin)
        uplugunload = plugRemove <$> bplugin <@> eplugunload
        uplugunload :: Event (Map String Plugin)

        send = hout . mangle

        -- einput :: Event (Stanza)
        bpluginlist = elems <$> bplugin
        bpluginlist :: Behavior ([Plugin])
        brunplugins = applyPlugins <$> bpluginlist
        brunplugins :: Behavior (Map Jid [Permissions] -> Handler Stanza -> Stanza -> IO ())
        bprunplugins = (flip ($)) <$> bpermission <*> brunplugins
        bprunplugins :: Behavior (Handler Stanza -> Stanza -> IO ())
        bpcrunplugins = (flip ($) send) <$> bprunplugins
        bpcrunplugins :: Behavior (Stanza -> IO ())

    reactimate $ fmap hplugin uplugload
    reactimate $ fmap hplugin uplugunload

    reactimate $ bpcrunplugins <@> einput

plugInsert :: Map String Plugin -> (String, Plugin) -> Map String Plugin
plugInsert = (flip . uncurry) insert

plugRemove :: Map String Plugin -> String -> Map String Plugin
plugRemove = flip delete

applyPlugins :: [Plugin] -> Map Jid [Permissions] -> Handler Stanza -> Stanza -> IO ()
applyPlugins [] _ _ _ = return ()
applyPlugins [x] p c s = applyPlugin x p c s
applyPlugins xs p c s = mapM_ (\x -> applyPlugin x p c s) xs

applyPlugin :: Plugin -> Map Jid [Permissions] -> Handler Stanza -> Stanza -> IO ()
applyPlugin x p c s = if plFilter x s then plAction x s (permFilter p s) c else return ()

permFilter :: Map Jid [Permissions] -> Stanza -> [Permissions]
permFilter p s = case extractJid s of
    Just j -> maybe [] id (lookup j p)
    Nothing -> []

sanitizeMuc :: Stanza -> Stanza
sanitizeMuc (MessageS m) = MessageS $ m { messageType = GroupChat }
sanitizeMuc s = id s

