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
import Gnut.Channel

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
        runCommandC = runCommand esplugin mangle hout defaults
        eemcommand = runCommandC <$> einput
        eemcommand :: Event (Maybe (Either (IO (Jid, Handler Stanza, EventNetwork)) (IO Jid)))

        eecommand = filterJust eemcommand
        eecommand :: Event (Either (IO (Jid, Handler Stanza, EventNetwork)) (IO Jid))

        (enetworkadd, enetworkdel) = split eecommand
        enetworkadd :: Event (IO (Jid, Handler Stanza, EventNetwork))
        enetworkdel :: Event (IO Jid)

        -- Send the networkupdate to the XMPP router
        enetup = liftM (\(j,h,_) -> Left (j,h)) <$> enetworkadd
        enetup :: Event (IO ChannelUpdate)
        enetdn = liftM Right <$> enetworkdel
        enetdn :: Event (IO ChannelUpdate)

        unetup = ((=<<) hchannel) <$> enetup
        unetup :: Event (IO ())
        unetdn = ((=<<) hchannel) <$> enetdn
        unetdn :: Event (IO ())

    reactimate unetup
    reactimate unetdn

        -- Save the EventNetwork
    let 
        enetadd = liftM (\(j,_,e) -> (j,e)) <$> enetworkadd
        enetadd :: Event (IO (Jid, EventNetwork))
        bnetadd = addNetwork <$> bnetwork
        bnetadd :: Behavior ((Jid, EventNetwork) -> M.Map Jid EventNetwork)
        bnetdel = delNetwork <$> bnetwork
        bnetdel :: Behavior (Jid -> M.Map Jid EventNetwork)
        unetadd = liftM <$> bnetadd <@> enetadd
        unetadd :: Event (IO (M.Map Jid EventNetwork))
        unetdel = liftM <$> bnetdel <@> enetworkdel
        unetdel :: Event (IO (M.Map Jid EventNetwork))

    reactimate $ ((=<<) hnetwork) <$> unetadd
    reactimate $ ((=<<) hnetwork) <$> unetdel


addNetwork = (flip . uncurry) M.insert
delNetwork = flip M.delete


runCommand :: AddHandler PlugUpdate
           -> (Stanza -> Stanza)
           -> Handler Stanza
           -> ChannelSettings
           -> ((Stanza, [Permissions]), Handler Stanza)
           -> Maybe (Either (IO (Jid, Handler Stanza, EventNetwork)) (IO Jid))
runCommand esplugin mangle hout defaults ((s, p), h) = case parseCommand s of
    Left _ -> Nothing
    Right c -> if checkPerm c p then Just $ runCommand' esplugin mangle hout defaults (c, h)
                                else Nothing

runCommand' :: AddHandler PlugUpdate
            -> (Stanza -> Stanza)
            -> Handler Stanza
            -> ChannelSettings
            -> (Command, Handler Stanza)
            -> Either (IO (Jid, Handler Stanza, EventNetwork)) (IO Jid)
runCommand' esplugin mangle hout defaults (Join j, h) = Left $ runJoin esplugin mangle hout defaults j h
runCommand' _ _ _ _ (Leave j, h) = Right $ runLeave j h

runJoin :: AddHandler PlugUpdate
        -> (Stanza -> Stanza)
        -> Handler Stanza
        -> ChannelSettings
        -> Jid
        -> Handler Stanza
        -> IO (Jid, Handler Stanza, EventNetwork)
runJoin esplugin mangle hout defaults j h = do
    (esinput, hinput) <- newAddHandler
    en <- setupChannelNetwork esinput esplugin mangle hout defaults
    actuate en
    return (j, hinput, en)

runLeave :: Jid
        -> Handler Stanza
        -> IO Jid
runLeave j h = (h $ leaveS j) >> return j

checkPerm :: Command -> [Permissions] -> Bool
checkPerm (Join _) p = checkAllOr False (PermPath ["admin","join"]) p
checkPerm (Leave _) p = checkAllOr False (PermPath ["admin","leave"]) p

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
