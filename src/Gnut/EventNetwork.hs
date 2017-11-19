module Gnut.EventNetwork
    ( globalEventNetwork
    )
    where

import Gnut.Types

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import Network.Xmpp
import Network.Xmpp.IM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

globalEventNetwork :: AddHandler Message -> MomentIO ()
globalEventNetwork messagehndl = do
    messages <- fromAddHandler messagehndl

    let 
        ims = filterJust $ apply (pure getIM) messages
        cmds = filterE isCommand ims
        joins = filterE isJoin ims

    reactimate $ print <$> messages
    --reactimate $ TIO.putStrLn . showImBody <$> ims


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

actuallyJoin :: Session -> IO ()
actuallyJoin sess = do
        _ <- sendPresence (Presence Nothing (Just $ parseJid "gnut@paranoidlabs.org") (Just $ parseJid "test2@chat.paranoidlabs.org/gnut") Nothing Available [] []) sess
        return ()
