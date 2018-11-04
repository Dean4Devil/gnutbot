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
import Gnut.Router
import Gnut.Module
import Gnut.Types
import Gnut.Permissions
import Gnut.Modules.Admin

run :: Config -> IO ()
run c = do
    session <- setupSession "paranoidlabs.org" (Just (const [plain ("gnut") Nothing ("quailaeQu3ahbei0vaXa")], Nothing))

    let perms = userPermsFromAccessConfig $ c^.access

    msgsrc <- newAddHandler
    pms <- newAddHandler
    a <- newAddHandler

    m <- setupMainNetwork session msgsrc perms pms a
    actuate m

    eventLoop msgsrc

    teardownSession session

{-
 -setupMainNetwork :: Session
 -                 -> EventSource Stanza
 -                 -> [User]
 -                 -> IO EventNetwork
 -}
setupMainNetwork s msgsrc perms pms a = compile $ do
    let mods = pureModuleStore ++ [(adminFilter, fire a)]
    let cs = dmChannelMap (fire pms) mods perms (fire msgsrc)

    (bchannel, hc) <- newBehavior cs
    (bmodules, hm) <- newBehavior mods

    liftIOLater $ do


        xmpp <- setupXmppNetwork s (addHandler msgsrc) bchannel
        pmrouter <- setupRouterNetwork id (addHandler pms) (fire msgsrc) bmodules perms

        adm <- setupAdminNetwork bchannel hc (addHandler a)

        actuate xmpp
        actuate pmrouter
        actuate adm

    return ()

eventLoop :: (EventSource Stanza) -> IO ()
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
                "d" -> fire esmsg $ MessageS $ simpleIM [jid|dean4devil@paranoidlabs.org|] (T.pack $ unwords xs)
                _ -> putStrLn "Commands: d <msg>|o <msg>|q"
            when (x /= "q") loop

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd
