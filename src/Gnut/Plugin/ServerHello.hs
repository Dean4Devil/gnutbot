module Gnut.Plugin.ServerHello
    ( plugin
    ) where

import Gnut.IRC

plugin :: NotYetPlugin
plugin = makeInitPlugin "ServerHello" [modeHook] initialize

initialize :: Irc ()
initialize = do
    nick <- getNick
    realName <- getRealName
    writeMessage "NICK" [nick]
    writeMessage "USER" ["Gnutbot", "*", "*", realName]

modeHook :: () -> Irc ()
modeHook _ = onCommand "376" $ do
    writeMessage "MODE" ["Gnut", "+B"]
    writeMessage "CAP REQ" [":", "away-notify", "extended-join", "account-notify"]
