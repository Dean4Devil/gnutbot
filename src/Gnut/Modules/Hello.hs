module Gnut.Modules.Hello
    ( getPlugin
    ) where

import System.IO

import Network.Xmpp.Internal hiding (Plugin)
import Network.Xmpp.IM

import Gnut.Types
import Gnut.Permissions

getPlugin :: Plugin
getPlugin = answerPlugin "Hello" messageFilter handleMessage

messageFilter = answerFilter helloFilter

helloFilter :: MessageBody -> Bool
helloFilter (MessageBody _ "!hai") = True
helloFilter (MessageBody _ _) = False


handleMessage _ _ = pure [MessageBody { bodyLang = Nothing, bodyContent = "Hello there!" }]
