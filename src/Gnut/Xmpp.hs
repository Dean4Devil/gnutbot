module Gnut.Xmpp
    ( setupSession
    , teardownSession
    , setupXmppNetwork
    , dmChannelMap
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent (forkIO)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Default
import Network.Socket (HostName)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Network.Xmpp
import Network.Xmpp.IM
import Network.Xmpp.Internal

setupSession :: HostName -> AuthData -> IO Session
setupSession domain authdata = do
    result <- session domain authdata $ def
        & streamConfigurationL .  tlsBehaviourL .~ RequireTls
        & onConnectionClosedL .~ reconnectSession
    sess <- case result of
            Right s -> return s
            Left e -> liftIO $ error $ "XMPP Failure: " ++ show e
    sendPresence presenceOnline sess
    return sess
  where
    reconnectSession sess failure = void (reconnect' sess)

teardownSession :: Session -> IO ()
teardownSession session = do
    sendPresence presenceOffline session
    endSession session

receiveLoop :: Session -> Handler Stanza -> IO ()
receiveLoop session h = do
    forkIO loop
    return ()
  where loop = do {
      (stanza, _) <- getStanza session;
      h stanza;
      loop;
  }

-- FIXME: Enforce there to always be a DirectMessage network by proper
-- types
data Channel = DirectMessage
             | MUC String
             deriving (Show, Eq, Ord)

setupXmppNetwork :: Session
                 -> AddHandler Stanza
                 -> Behavior (Map Channel (Handler Stanza))
                 -> IO EventNetwork
setupXmppNetwork session esout bchannels = compile $ do
    -- Stanzas some part of Gnut wants to send in eout
    eout <- fromAddHandler esout

    -- Send all stanzas that are to be sent
    reactimate $ fmap (sendStanza_ session) eout

    -- Input of all stanzas that are received in ein
    (ein, hin) <- newEvent
    liftIOLater $ receiveLoop session hin

    -- TODO: This valueB could possibly be replaced by `apply` on the ea/eb
    -- step? Is that more efficient/smarter?
    channels <- valueB bchannels
    let
        ea = fmap (\s -> (getChannel s, s)) ein
        ea :: Event (Channel, Stanza)

        eb = fmap (\(c, s) -> (M.lookup c channels, s)) ea
        eb :: Event (Maybe (Handler Stanza), Stanza)

        ec = fmap innerMaybe eb
        ec :: Event (Maybe (Handler Stanza, Stanza))

        ed = filterJust ec
        ed :: Event (Handler Stanza, Stanza)

        ee = fmap (\(h, s) -> h s) ed
        ee :: Event (IO ())

    reactimate ee

sendStanza_ :: Session -> Stanza -> IO ()
sendStanza_ session stanza = do
    _ <- sendStanza stanza session
    return ()

-- FIXME
getChannel :: Stanza -> Channel
getChannel s = DirectMessage

innerMaybe :: (Maybe a, b) -> Maybe (a,b)
innerMaybe (Just a, b) = Just (a,b)
innerMaybe _ = Nothing

dmChannelMap :: Handler Stanza -> Behavior (Map Channel (Handler Stanza))
dmChannelMap h = pure m
    where m = M.fromList [(DirectMessage, h)]
