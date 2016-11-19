module Gnut.MsgCommands
    ( onMsgCommand
    , onMsgCommands
    , getMsgCommand
    , getMsgCommandText
    ) where

import              Control.Monad   (forM_, when)
import              Data.Char       (isSpace)
import              Data.Text       (Text)
import qualified    Data.Text       as T

import              Gnut.Message
import              Gnut.IRC

getMsgCommand :: Irc Text
getMsgCommand = flip fmap getMessageText $ T.takeWhile (not . isSpace)

getMsgCommandText :: Irc Text
getMsgCommandText = flip fmap getMessageText $ T.strip . T.dropWhile (not . isSpace)

onMsgCommand :: Text -> Irc () -> Irc ()
onMsgCommand command = onMsgCommands [command]

onMsgCommands :: [Text] -> Irc () -> Irc ()
onMsgCommands commands irc = onCommand "PRIVMSG" $ do
    cmd <- getMsgCommand
    forM_ commands $ \c -> when (cmd ==? c) irc
