module Gnut
    ( Gnut
    , runGnut
    , GnutS(..)

    , eventLoop

    , parseConfig
    , Config(..)
    , setupSession
    , getMessage

    , Command
    , setupNetwork
    , setupAll
    )
    where

import Gnut.Types
import Gnut.Config
import Gnut.Xmpp
import Gnut.Command.Xmpp
import Gnut.Tui


import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Control.Event.Handler

import Data.Maybe
import Data.Map (fromList)

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Network.Xmpp as X
import Network.Xmpp.IM

import Data.Text (Text)

eventLoop :: Gnut ()
eventLoop = do
    forkIO $ xmppLoop . globalHndl <$> get
    tuiLoop

{-
 -withGnut :: FilePath -> IO ()
 -withGnut configpath = do
 -    config <- fromJust <$> parseConfig configpath
 -    sess <- setupSession config
 -    hndl <- setupAll sess
 -
 -    let gnuts = GnutS (fromList []) hndl sess
 -
 -    _ <- runGnut config gnuts eventLoop
 -
 -    return ()
 -
 -eventLoop :: Gnut ()
 -eventLoop = do
 -    s <- get
 -    let se = gnutSession s
 -        hndl = globalHndl s
 -    forever $ do
 -        msg <- getMessage se
 -        hndl msg
 -    return ()
 -}

setupAll :: Session -> IO (Message -> IO ())
setupAll sess = do
    (addHandler, fire) <- newAddHandler
    network <- setupNetwork sess addHandler
    actuate network
    return fire


setupNetwork :: Session -> AddHandler Message -> IO EventNetwork
setupNetwork sess esmsg = compile $ do
    emsg <- fromAddHandler esmsg

    let ecmd = filterJust $ apply parseMessageB emsg

    reactimate $ fmap print ecmd
    reactimate $ fmap (evalCommand sess) ecmd

evalCommand :: Session -> Command -> IO ()
evalCommand sess ("^Hai", m) = 
        void (X.sendMessage (fromJust (answerIM [MessageBody Nothing "Hai!" ] m)) sess)
evalCommand sess ("^HowDoing", m) =
        void (X.sendMessage (fromJust (answerIM [MessageBody Nothing "Baby steps...." ] m)) sess)
evalCommand sess (_, m) =
        void (X.sendMessage (fromJust (answerIM [MessageBody Nothing "I don't know that command, sorry."] m)) sess)
