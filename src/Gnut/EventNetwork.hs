module Gnut.EventNetwork
    ( routerNetwork
    )
    where

import Gnut.Types
import Gnut.Xmpp

import Data.Set (Set)
import qualified Data.Set as Set

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Set of ignored JIDs. If the Sender is in the Set all events are directly
-- ignored
ignoreF :: Set Jid -> Message -> Maybe Message
ignoreF s m@(Message _ (Just jid) _ _ _ [] []) = if jid `Set.member` s then Nothing else Just m
ignoreF s m@(Message _ Nothing _ _ _ [] []) = Just m

routerNetwork :: Session -> AddHandler Message -> MomentIO ()
routerNetwork sess source = do
    messages <- fromAddHandler source
    (ignore, replaceIgnore) <- newBehavior (ignoreF (Set.singleton $ parseJid "test@paranoidlabs.org"))

    let filtered = filterJust $ apply ignore messages

        ignoreCmd = filterE isIgnore $ filterE isCommand $ filterJust $ apply (pure getIM) messages

        ims = filterJust $ apply (pure getIM) filtered
        cmds = filterE isCommand ims

        joins = filterE isJoin cmds

    reactimate $ print <$> filtered
    reactimate $ actuallyJoin sess <$ joins

isCommand :: InstantMessage -> Bool
isCommand (InstantMessage _ _ body) = isCommandBody (head body)

isCommandBody :: MessageBody -> Bool
isCommandBody (MessageBody _ cont) = T.head cont == '!'

showImBody :: InstantMessage -> Text
showImBody (InstantMessage _ _ body) = showImBody' (head body)
    where showImBody' (MessageBody _ cont) = cont

isJoin :: InstantMessage -> Bool
isJoin im = head bs == "!join"
    where b = showImBody im
          bs = T.words b

isIgnore :: InstantMessage -> Bool
isIgnore im = head bs == "!ignore"
    where b = showImBody im
          bs = T.words b

actuallyJoin :: Session -> IO ()
actuallyJoin sess = do
        _ <- sendPresence (Presence Nothing (Just $ parseJid "gnut@paranoidlabs.org") (Just $ parseJid "test@chat.paranoidlabs.org/gnut") Nothing Available [] []) sess
        return ()
