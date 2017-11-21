module Gnut
    ( Gnut
    , runGnut
    , GnutState(..)

    , parseConfig
    , Config(..)
    , setupSession
    , getMessage

    , run
    )
    where

import Gnut.Types
import Gnut.Config
import Gnut.Xmpp
import Gnut.EventNetwork
import Gnut.Tui
import Gnut.DynamicLoad

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import System.IO hiding (putStrLn, print)

run :: Session -> IO ()
run sess = do
    (lineH, sendLine) <- newAddHandler
    (loadH, load) <- newAddHandler

    (messageH, sendMessage) <- newAddHandler
    (ignoreH, sendIgnore) <- newAddHandler

    let avail = Map.singleton "echo" print

    let cmds = Map.fromList [ ("send", sendM sess)
                            , ("quit", \_ -> return ())
                            , ("load", cmdLoadModule avail load)
                            , ("unload", cmdUnloadModule load)
                            , ("ignore", ignoreU sendIgnore)
                            , ("unignore", unignoreU sendIgnore)
                            ]

    let ignores = Set.empty

    tui <- compile $ tuiNetwork cmds loadH lineH
    router <- compile $ routerNetwork ignores ignoreH messageH

    actuate tui
    actuate router

    forkIO $ xmppLoop sess sendMessage router
    tuiLoop sendLine tui

tuiLoop :: Handler Text -> EventNetwork -> IO ()
tuiLoop fire network = loop
  where
    loop = do
        putStr "> "
        hFlush stdout
        hSetBuffering stdin NoBuffering
        l <- TIO.getLine
        fire l
        when (l /= "quit") loop

xmppLoop :: Session -> Handler Message -> EventNetwork -> IO ()
xmppLoop sess fire network = loop
  where
    loop = do
        m <-getMessage sess
        fire m
        loop

sendM :: Session -> [Text] -> IO ()
sendM sess [] = putStrLn "send <jid> <message>"
sendM sess [x] = putStrLn "send <jid> <message>"
sendM sess (jid:text) = case jidFromText jid of
    Nothing -> putStrLn "Invalid JID"
    Just j -> do
        r <- sendMessage (simpleIM j (T.unwords text)) sess
        return ()

ignoreU :: Handler IgnoreEvent -> [Text] -> IO ()
ignoreU h [x] = case jidFromText x of
    Nothing -> putStrLn "invalid JID"
    Just j -> h $ Left j
ignoreU _ _ = putStrLn "ignore <jid>"

unignoreU :: Handler IgnoreEvent -> [Text] -> IO ()
unignoreU h [x] = case jidFromText x of
    Nothing -> putStrLn "invalid JID"
    Just j -> h $ Right j
unignoreU _ _ = putStrLn "unignore <jid>"
