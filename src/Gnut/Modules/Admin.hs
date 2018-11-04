module Gnut.Modules.Admin
    ( setupAdminNetwork
    , adminFilter
    ) where

import Prelude hiding (words)

import Network.Xmpp
import Network.Xmpp.Internal

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

setupAdminNetwork :: Behavior ChannelNetworks
                  -> Handler (ChannelNetworks)
                  -> AddHandler ((Stanza, [Permissions]), Handler Stanza)
                  -> IO EventNetwork
setupAdminNetwork bchannels hchannels escommands = compile $ do
    -- Stanzas that have been sent to us
    ecommands <- fromAddHandler escommands

    let 
        curriedChanUpdate = updateChan hchannels
        curriedChanUpdate :: ChannelNetworks -> Command -> IO ()
        chanUpdateB = fmap curriedChanUpdate bchannels
        chanUpdateB :: Behavior (Command -> IO ())
        currChans = fmap (M.keys . nws) bchannels
        currChans :: Behavior ([Jid])

    let 
        eec = fmap parseCommand' ecommands
        eec' = fmap (\(e, h) -> innerEither (innerEither e, h)) eec
        (ee, ec) = split eec'

        ecc = fmap checkPerm ec
        ecc' = filterJust ecc

        ecmd = fmap (fst) ecc'
        ecmd :: Event (Command)

        ecmdio = chanUpdateB <@> ecmd
        ecmdio :: Event (IO ())

        ecc'' = fmap (\(c, h) -> h $ toResponse c) ecc'

        ecurrc = currChans <@ ecmd

    reactimate ecc''
    reactimate ecmdio


sendChanUpdate :: Handler ChannelNetworks -> ChannelNetworks -> IO ()
sendChanUpdate h u = h u

updateChan :: Handler ChannelNetworks -> ChannelNetworks -> Command -> IO ()
updateChan h cs c = do
    newcs <- updateChannels cs c
    sendChanUpdate h newcs

updateChannels :: ChannelNetworks -> Command -> IO ChannelNetworks
updateChannels cs (Join j') = do
    let 
        bplugins = defMods cs
        uperm = defPerms cs
        enmap = nws cs
        j = toBare j'

    (esin, hin) <- newAddHandler

    newnw <- setupRouterNetwork mangleMuc esin (hout cs) (pure $ bplugins) uperm
    actuate newnw

    let hs = simpleIM j "Hello there, I am Gnut, your friendly neighbourhood bot!"
        hs' = MessageS $ hs{ messageType = GroupChat }
    hout cs $ hs'


    let cs' = cs { nws = M.insert j newnw (nws cs)
                 , muc = M.insert j hin (muc cs)
                 }

    return cs'


updateChannels cs (Leave j') = case M.lookup j (nws cs) of
    Just nw -> do
        pause nw
        let cs' = cs { nws = M.delete j (nws cs)
                     , muc = M.delete j (muc cs)
                     }
        return cs'
    Nothing -> return cs
  where j = toBare j'

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
