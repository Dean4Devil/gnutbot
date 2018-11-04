module Gnut.Modules.Admin
    ( setupAdminNetwork
    ) where

import Prelude hiding (words)

import Network.Xmpp.Internal

import Data.Text

import Reactive.Banana
import Reactive.Banana.Frameworks

import Gnut.Xmpp

data Command = Join Jid
              | Leave Jid

data CError = InvalidCommand
            | InvalidJid


setupAdminNetwork :: Handler (ChannelNetworks)
                  -> AddHandler Stanza
                  -> IO EventNetwork
setupAdminNetwork hchannels escommands = compile $ do
    -- Stanzas that have been sent to us
    ecommands <- fromAddHandler escommands

    return ()

parseCommand :: Stanza -> Either CError Command
parseCommand (MessageS m) = parseMessageCommand $ getIM m
parseCommand _ = Left InvalidCommand

parseMessageCommand :: Maybe InstantMessage -> Either CError Command
parseMessageCommand Nothing = Left InvalidCommand
parseMessageCommand (Just (InstantMessage _ _ [])) = Left InvalidCommand
parseMessageCommand (Just (InstantMessage _ _ ((MessageBody _ c):_))) = parseBodyCommand c

parseBodyCommand :: Text -> Either CError Command
parseBodyCommand c | Just rest <- stripPrefix "!join" c = parseJoin $ words rest
parseBodyCommand c | Just rest <- stripPrefix "!leave" c = parseLeave $ words rest
parseBodyCommand _ = Left InvalidCommand

parseJoin :: [Text] -> Either CError Command
parseJoin [] = Left InvalidJid
parseJoin [x] = maybe (Left InvalidJid) (\x -> Right (Join x)) $ jidFromText x

parseLeave :: [Text] -> Either CError Command
parseLeave [] = Left InvalidJid
parseLeave [x] = maybe (Left InvalidJid) (\x -> Right (Leave x)) $ jidFromText x
