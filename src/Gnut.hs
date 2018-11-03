{-# LANGUAGE QuasiQuotes #-}
module Gnut (run) where

import System.IO
import Control.Monad (when)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Text (Text)
import qualified Data.Text as T

import Network.Xmpp.Internal

import Gnut.Xmpp
import Gnut.Router
import Gnut.Module
import Gnut.Types
import Gnut.Permissions

run :: IO ()
run = do
    msgsrc <- newAddHandler
    pms <- newAddHandler
    session <- setupSession "paranoidlabs.org" (Just (const [plain ("gnut") Nothing ("quailaeQu3ahbei0vaXa")], Nothing))
    xmpp <- setupXmppNetwork session (addHandler msgsrc) (dmChannelMap (fire pms))
    pmrouter <- setupRouterNetwork (addHandler pms) (fire msgsrc) pureModuleStore

    actuate xmpp
    actuate pmrouter

    eventLoop msgsrc

    teardownSession session

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
