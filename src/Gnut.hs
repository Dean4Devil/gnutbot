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
import Gnut.Xmpp
import Gnut.Channel
import Gnut.Module
import Gnut.Types
import Gnut.Permissions
import Gnut.Modules.Admin

run :: Config -> IO ()
run c = do
    session <- setupSession "paranoidlabs.org" (Just (const [plain ("gnut") Nothing ("quailaeQu3ahbei0vaXa")], Nothing))

    let perms = userPermsFromAccessConfig $ c^.access
        chansettings = ChannelSettings { csPlugins = M.empty, csPermissions = perms }

    (esstanza, hstanza) <- newAddHandler
    (eschannel, hchannel) <- newAddHandler
    (esprivmsg, hprivmsg) <- newAddHandler
    (esplugin, hplugin) <- newAddHandler

    xmpp <- setupXmppNetwork session esstanza eschannel hprivmsg
    privmsg <- setupChannelNetwork esprivmsg esplugin id hstanza chansettings

    actuate privmsg
    actuate xmpp

    eventLoop hstanza

    teardownSession session

{-
 -{-
 - -setupMainNetwork :: Session
 - -                 -> EventSource Stanza
 - -                 -> [User]
 - -                 -> IO EventNetwork
 - -}
 -setupMainNetwork s msgsrc perms pms a = compile $ do
 -    let mods = pureModuleStore ++ [(adminFilter, fire a)]
 -    let cs = dmChannelMap (fire pms) mods perms (fire msgsrc)
 -
 -    (bchannel, hc) <- newBehavior cs
 -    (bmodules, hm) <- newBehavior mods
 -
 -    liftIOLater $ do
 -
 -
 -
 -        adm <- setupAdminNetwork bchannel hc (addHandler a)
 -
 -    return ()
 -}

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
