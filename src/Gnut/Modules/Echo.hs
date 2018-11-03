module Gnut.Modules.Echo
    ( getPlugin
    ) where

import Prelude hiding (drop)

import Network.Xmpp.Internal hiding (Plugin)
import Network.Xmpp.IM

import Data.Text hiding (head)

import Gnut.Types
import Gnut.Permissions

getPlugin :: Plugin
getPlugin = answerPlugin "Echo" messageFilter handleMessage

messageFilter = commandFilter "!echo"


handleMessage :: InstantMessage -> [Permissions] -> IO [MessageBody]
handleMessage (InstantMessage _ _ []) _ = return []
handleMessage (InstantMessage _ _ xs) p = case checkAllOr False (PermPath ["gnut", "echo"]) p of
        True -> return $ [echoBody $ head xs]
        False -> return [permissionDenied]

echoBody :: MessageBody -> MessageBody
echoBody (MessageBody _ "") = MessageBody { bodyLang = Nothing
                    , bodyContent = "You need to tell me something to echo..." }
echoBody (MessageBody _ "!echo") = MessageBody { bodyLang = Nothing
                    , bodyContent = "You need to tell me something to echo..." }
echoBody (MessageBody _ xs) = MessageBody { bodyLang = Nothing
                    , bodyContent = drop 5 xs }

permissionDenied :: MessageBody
permissionDenied =  MessageBody { bodyLang = Nothing
                    , bodyContent = "No." }
