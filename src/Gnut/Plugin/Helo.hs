module Gnut.Plugin.Helo
    ( plugin
    ) where

import Gnut.IRC
import Gnut.MsgCommands

plugin :: NotYetPlugin
plugin = makePlugin "Helo" [heloHook]

heloHook :: Irc ()
heloHook = onMsgCommand "^hello" $ do
    noticeReply "Hai there! ^.^"
