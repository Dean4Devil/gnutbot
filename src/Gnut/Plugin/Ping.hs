
module Gnut.Plugin.Ping
    ( plugin
    ) where

import Gnut.IRC

plugin :: NotYetPlugin
plugin = makePlugin "Ping" [pingHook]

pingHook :: Irc ()
pingHook = onCommand "PING" $ do
    param <- getParameters
    writeMessage "PONG" param
