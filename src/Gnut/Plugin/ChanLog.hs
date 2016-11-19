module Gnut.Plugin.ChanLog
    ( plugin
    ) where

import qualified    Database.SQLite.Simple  as SQLite

import Gnut.IRC
import Gnut.MsgCommands

plugin :: NotYetPlugin
plugin = makeInitPlugin "ChanLog" [logHook] initialize

initialize :: Irc ()
initialize = withDatabase $ \db -> SQLite.execute_ db
    "CREATE TABLE IF NOT EXISTS channellog ( \
    \   id      INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \
    \   channel TEXT                              NOT NULL, \
    \   nick    TEXT                              NOT NULL, \
    \   message TEXT                              NOT NULL  \
    \)"

logHook :: () -> Irc ()
logHook _ = onCommand "PRIVMSG" $ do
    channel <- getChannel
    nick    <- getSender
    message <- getMessageText

    withDatabase $ \db -> SQLite.execute db
        "INSERT INTO channellog (channel, nick, message) VALUES (?, ?, ?)"
        (channel, nick, message)
