{-# LANGUAGE QuasiQuotes #-}
module Gnut
    ( run
    , parseConfig
    ) where

import System.IO
import Control.Monad (when)

import Control.Lens

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

import Network.Xmpp.Internal

import Gnut.Config
import Gnut.Interface
import Gnut.Xmpp
import Gnut.Channel
import Gnut.Types
import Gnut.Permissions

import Gnut.Modules.Admin

import qualified Gnut.Modules.Echo as Echo
import qualified Gnut.Modules.Hello as Hello

run :: Config -> IO ()
run c = do
    session <- setupSession "paranoidlabs.org" (Just (const [plain ("gnut") Nothing ("quailaeQu3ahbei0vaXa")], Nothing))

    (esstanza, hstanza) <- newAddHandler
    (eschannel, hchannel) <- newAddHandler
    (esprivmsg, hprivmsg) <- newAddHandler
    (espmplugin, hpmplugin) <- newAddHandler
    (esplugin, hplugin) <- newAddHandler
    (eschanplugin, hchanplugin) <- newAddHandler
    (esadmin, hadmin) <- newAddHandler

    let perms = userPermsFromAccessConfig $ c^.access
        chansettings = ChannelSettings
                     { csPlugins = M.fromList [("Admin", adminPlugin hadmin), ("Echo", Echo.getPlugin), ("Hello", Hello.getPlugin)]
                     , csPermissions = perms
                     }

    print perms

    xmpp <- setupXmppNetwork session esstanza eschannel hprivmsg
    privmsg <- setupChannelNetwork esprivmsg espmplugin id hstanza chansettings

    admin <- setupAdminNetwork hchannel esadmin hchanplugin mangleMuc (hstanza . mangleMuc) chansettings
    iface <- setupInterfaceNetwork hpmplugin eschanplugin esplugin

    actuate admin

    actuate privmsg
    actuate xmpp

    eventLoop hstanza

    teardownSession session

mangleMuc :: Stanza -> Stanza
mangleMuc (MessageS m) = MessageS $ m { messageType = GroupChat
                                      , messageTo = fmap toBare (messageTo m)
                                      }
mangleMuc o = o

eventLoop :: Handler Stanza -> IO ()
eventLoop esmsg = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        s <- getLine
        if s == "" then do
            putStrLn "Commands: d <msg>|o <msg>|q"
            loop
        else do
            let (x:xs) = words s
            case x of
                "q" -> return ()
                "d" -> esmsg $ MessageS $ simpleIM [jid|dean4devil@paranoidlabs.org|] (T.pack $ unwords xs)
                _ -> putStrLn "Commands: d <msg>|o <msg>|q"
            when (x /= "q") loop
