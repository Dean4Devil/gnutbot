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
    --forkIO $ xmppLoop . globalHndl <$> get
    liftIO $ tuiLoop

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
