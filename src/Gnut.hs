{-# LANGUAGE QuasiQuotes #-}
module Gnut (run) where

import System.IO
import Control.Monad (when)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Text (Text)
import qualified Data.Text as T

import Network.Xmpp.Internal

import Gnut.Router
import Gnut.Types
import Gnut.Permissions

run :: IO ()
run = do
    msgsrc <- newAddHandler
    network <- setupRouterNetwork purePerm (addHandler msgsrc)
    actuate network
    eventLoop msgsrc

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
                "o" -> fire esmsg $ MessageS $ simpleIM [jid|cli@gnut|] (T.pack $ unwords xs)
                "d" -> fire esmsg $ MessageS $ simpleIM [jid|cli@gnut|] (T.pack $ unwords xs)
                _ -> putStrLn "Commands: d <msg>|o <msg>|q"
            when (x /= "q") loop

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd
