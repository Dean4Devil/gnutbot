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

setupXmppNetwork :: Session
                 -> AddHandler Stanza
                 -> Behavior ChannelNetworks
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
        mucPmB = fmap (\c s -> (eitherMucPm c s, s)) bchannels
        mucPmB :: Behavior (Stanza -> (Either (Handler Stanza) (Handler Stanza), Stanza))

        ea = mucPmB <@> ein
        ea :: Event (Either (Handler Stanza) (Handler Stanza), Stanza)

        eb = fmap innerEither ea
        eb :: Event (Either (Handler Stanza, Stanza) (Handler Stanza, Stanza))

        (emuc,epm) = split eb
        epm :: Event (Handler Stanza, Stanza)
        emuc :: Event (Handler Stanza, Stanza)

        epmrecvd = fmap (\(h, s) -> h s) epm
        epmrecvd :: Event (IO ())

        emucrecvd = fmap (\(h, s) -> h s) emuc
        emucrecvd :: Event (IO ())

    reactimate epmrecvd
    reactimate emucrecvd
    reactimate $ fmap (putStrLn . ((++) "Sending: ") . show) eout
    reactimate $ fmap (putStrLn . ((++) "Rcvd via MUC: ") . show . snd) emuc
    reactimate $ fmap (putStrLn . ((++) "Rcvd via PM: ") . show . snd) epm
    reactimate $ fmap (print) (bchannels <@ ein)

-- Left Muc | Right PM/Not joined Muc
eitherMucPm :: ChannelNetworks -> Stanza -> Either (Handler Stanza) (Handler Stanza)
eitherMucPm c s = do
    case extractJid s of
        --Just j -> case M.lookup (toBare j) (muc c) of
        Just j -> case M.lookup (toBare j) (muc c) of
            Just h -> Left h
            Nothing -> Right $ directMessage c
        Nothing -> Right $ directMessage c

sendStanza_ :: Session -> Stanza -> IO ()
sendStanza_ session stanza = do
    _ <- sendStanza stanza session
    return ()

sanitizeMuc :: Stanza -> Stanza
sanitizeMuc (MessageS m) = MessageS $ m { messageType = GroupChat }
sanitizeMuc s = id s

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
