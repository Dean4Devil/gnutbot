module Gnut.Plugin.Error
    ( plugin
    ) where

import Control.Monad.Trans (liftIO)

import Gnut.IRC
import Gnut.MsgCommands
import Gnut.Error

plugin :: NotYetPlugin
plugin = makePlugin "Error" [errHook]

errHook :: Irc ()
errHook = onMsgCommand "^err" $ do
    noticeReply =<< liftIO randomError
