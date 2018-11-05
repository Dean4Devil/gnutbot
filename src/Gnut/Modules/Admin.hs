module Gnut.Modules.Admin
    ( setupAdminNetwork
    , adminFilter
    ) where

import Prelude hiding (words)

import Network.Xmpp
import Network.Xmpp.Internal

import Control.Monad

import Data.Text hiding (split, any)
import Data.Maybe

import qualified Data.Map as M

import Reactive.Banana
import Reactive.Banana.Frameworks

import Gnut.Types
import Gnut.Xmpp
import Gnut.Permissions
import Gnut.Router

data Command = Join Jid
             | Leave Jid
             deriving (Show, Eq, Ord)

data CError = InvalidCommand
            | InvalidJid
            deriving (Show, Eq, Ord)


adminFilter = simpleFilter $ answerFilter $ (\b -> or [commandFilter' "!join" b, commandFilter' "!leave" b])


type ChannelUpdate = Either (Jid, Handler Stanza) Jid

setupAdminNetwork :: Handler ChannelUpdate
                   -> AddHandler ((Stanza, [Permissions]), Handler Stanza)
                   -> AddHandler PlugUpdate
                   -> (Stanza -> Stanza)
                   -> Handler Stanza
                   -> ChannelSettings
                   -> IO EventNetwork
setupAdminNetwork hchannel esinput esplugin mangle hout defaults = compile $ do
    einput <- fromAddHandler esinput

    (bnetwork, hnetwork) <- newBehavior M.empty

    let 
        runCommand' = runCommand esplugin mangle hout defaults
        eecommand' = runCommand' <$> einput
        eecommand' :: Event (Maybe (Either (IO (EventNetwork, (Jid, Handler Stanza))) Jid))

        eecommand = filterJust eecommand'

        echanupdate = fmap (either (fmap $ Left . snd) (return . Right)) eecommand
        echanupdate :: Event (IO ChannelUpdate)

        uchanupdate = fmap (hchannel =<<) echanupdate
        uchanupdate :: Event (IO ())

        (ecreate, eremove) = split eecommand
        ecreate :: Event (IO (EventNetwork, (Jid, Handler Stanza)))
        eremove :: Event Jid

        eeventnetworkcreate = fmap (fmap $ \(e, (j, _)) -> (j, e)) ecreate
        eeventnetworkcreate :: Event (IO (Jid, EventNetwork))

        bnetworkAdd = fmap liftM (networkAdd <$> bnetwork)
        bnetworkAdd :: Behavior (IO (Jid, EventNetwork) -> IO (M.Map Jid EventNetwork))
        enetworkAdd = bnetworkAdd <@> eeventnetworkcreate
        enetworkAdd :: Event (IO (M.Map Jid EventNetwork))
        unetworkAdd = fmap (hnetwork =<<) enetworkAdd
        unetworkAdd :: Event (IO ())

        bnetworkDel = networkDel' <$> bnetwork
        bnetworkDel :: Behavior (Jid -> IO (M.Map Jid EventNetwork))
        enetworkDel = bnetworkDel <@> eremove
        enetworkDel :: Event (IO (M.Map Jid EventNetwork))
        unetworkDel = fmap (hnetwork =<<) enetworkDel
        unetworkDel :: Event (IO ())


    reactimate uchanupdate
    reactimate unetworkAdd
    reactimate unetworkDel


networkAdd = (flip . uncurry) M.insert

--networkDel = flip M.delete

networkDel' :: M.Map Jid EventNetwork -> Jid -> IO (M.Map Jid EventNetwork)
networkDel' m j = do
    maybe (return ()) pause (M.lookup j m)
    return $ M.delete j m

runCommand :: AddHandler PlugUpdate
           -> (Stanza -> Stanza)
           -> Handler Stanza
           -> ChannelSettings 
           -> ((Stanza, [Permissions]), Handler Stanza)
           -> Maybe (Either (IO (EventNetwork, (Jid, Handler Stanza))) Jid)
runCommand esplugin mangle hout defaults abomination = case parseCommand' abomination of
    ((Left _, _), _) -> Nothing
    ((Right c, p), h) -> case checkPerm ((c, p), h) of
        Nothing -> Nothing
        Just (Leave j, h) -> Just $ Right j
        Just (Join j, h) -> Just $ Left $ do
            


parseCommand' :: ((Stanza, [Permissions]), Handler Stanza)
              -> ((Either CError Command, [Permissions]), Handler Stanza)
parseCommand' ((s,p), h) = ((parseCommand s, p), h)

checkPerm :: ((Command, [Permissions]), Handler Stanza) -> Maybe (Command, Handler Stanza)
checkPerm ((c@(Join _), p), h) = if checkAllOr False (PermPath ["admin","join"]) p == True then Just (c, h) else Nothing
checkPerm ((c@(Leave _), p), h) = if checkAllOr False (PermPath ["admin","leave"]) p == True then Just (c, h) else Nothing

toResponse :: Command -> Stanza
toResponse (Join j) = joinS j
toResponse (Leave j) = leaveS j

joinS = PresenceS . presTo presenceOnline . anyJidToNick
leaveS = PresenceS . presTo presenceOffline . anyJidToNick

anyJidToNick :: Jid -> Jid
anyJidToNick = fromJust . (uncurry3 jidFromTexts) . (\(a,b,c) -> (a,b, Just "gnut")) . jidToTexts
  where uncurry3 = (\f (a,b,c) -> f a b c)

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
