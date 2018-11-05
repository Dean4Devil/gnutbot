module Gnut.Xmpp
    ( setupSession
    , teardownSession
    , setupXmppNetwork
    , dmChannelMap
    , ChannelNetworks(..)
    , innerEither
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

import Gnut.Types
import Gnut.Module
import Gnut.Permissions

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

-- Left = Join, Right = Leave
type ChannelUpdate = Either (Jid, Handler Stanza) Jid

setupXmppNetwork :: Session
                 -> AddHandler Stanza
                 -> AddHandler ChannelUpdate
                 -> Handler Stanza
                 -> IO EventNetwork
setupXmppNetwork session esoutput eschannel pmchannel = compile $ do
    -- Stanzas we need to send to the world
    eoutput <- fromAddHandler esoutput

    -- Input of all stanzas that are received in ein
    (einput, hinput) <- newEvent
    liftIOLater $ receiveLoop session hinput

    -- Channel Join/Leaves
    echannel <- fromAddHandler eschannel

    (bchannel, hchannel) <- newBehavior M.empty

    let 
        (echanjoin, echanleave) = split echannel

        uchanjoin = chanJoin <$> bchannel <@> echanjoin
        uchanjoin :: Event (Map Jid (Handler Stanza))
        uchanleave = chanLeave <$> bchannel <@> echanleave
        uchanleave :: Event (Map Jid (Handler Stanza))

        -- bchannel :: Behavior (Map Jid (Handler Stanza))
        bsendchannel = sendChannel <$> bchannel
        bsendchannel :: Behavior (Handler Stanza -> Stanza -> IO ())
        bwithpmchannel = (flip ($) pmchannel) <$> bsendchannel
        bwithpmchannel :: Behavior (Stanza -> IO ())

    reactimate $ fmap (sendStanza_ session) eoutput
    reactimate $ fmap hchannel uchanjoin
    reactimate $ fmap hchannel uchanleave
    reactimate $ bwithpmchannel <@> einput

chanJoin :: Map Jid (Handler Stanza) -> (Jid, (Handler Stanza)) -> Map Jid (Handler Stanza)
chanJoin = (flip . uncurry) M.insert

chanLeave :: Map Jid (Handler Stanza) -> Jid -> Map Jid (Handler Stanza)
chanLeave = flip M.delete

chanLookup :: Map Jid (Handler Stanza) -> Jid -> Maybe (Handler Stanza)
chanLookup = flip M.lookup

sendChannel :: Map Jid (Handler Stanza) -> Handler Stanza -> Stanza -> IO ()
sendChannel m p s = maybe p id (chanLookup m =<< extractJid s) s

sendStanza_ :: Session -> Stanza -> IO ()
sendStanza_ session stanza = do
    _ <- sendStanza stanza session
    return ()

innerEither :: (Either a b, c) -> Either (a,c) (b,c)
innerEither (Left a, c) = Left (a,c)
innerEither (Right b, c) = Right (b,c)

data ChannelNetworks = ChannelNetworks
    { directMessage :: (Handler Stanza)
    , muc :: Map Jid (Handler Stanza)
    , nws :: Map Jid (EventNetwork)
    , hout :: Handler Stanza
    , defMods :: [Module]
    , defPerms :: Map Jid Permissions
    }

dmChannelMap :: Handler Stanza
             -> [Module]
             -> Map Jid Permissions
             -> Handler Stanza
             -> ChannelNetworks
dmChannelMap h m p out = ChannelNetworks
    { directMessage = h
    , muc = M.empty
    , nws = M.empty
    , hout = out
    , defMods = m
    , defPerms = p
    }

instance Show ChannelNetworks where
    show (ChannelNetworks _ muc nws _ _ defPerms) =
        "ChannelNetworks ( Joined MUC: " ++ (show $ M.keys muc) ++ ", Networks: " ++ (show $ M.keys nws) ++ ", default Perms: " ++ show defPerms ++ ")\n"
