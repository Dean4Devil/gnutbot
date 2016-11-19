module Gnut.Plugin.MessageStore
    ( plugin
    ) where

import qualified    Database.SQLite.Simple  as SQLite
import              Database.SQLite.Simple.FromField
import              Database.SQLite.Simple.Ok
import              Database.SQLite.Simple.ToField
import              Data.Char               (isSpace)
import              Data.Text               (Text)
import qualified    Data.Text               as T
import              Control.Arrow           (second)
import              Control.Concurrent      (forkIO, threadDelay)
import              Control.Monad           (forM_, unless)
import              Control.Monad.Trans     (liftIO)
import              Data.Time.Clock         (UTCTime, getCurrentTime)

import              Gnut.Message
import              Gnut.IRC
import              Gnut.Error
import              Gnut.MsgCommands

plugin :: NotYetPlugin
plugin = makeInitPlugin "MessageStore" [storeHook, checkHook, activityHook] initialize

data StoredMessageType = Ask | Tell deriving (Show)
data StoredMessage = StoredMessage
    { mHost      :: Text
    , mChannel   :: Text
    , mSender    :: Text
    , mRecipient :: Text
    , mTime      :: UTCTime
    , mMsg       :: Text
    , mType      :: StoredMessageType
    } deriving (Show)

instance FromField StoredMessageType where
    fromField f = cvt f . fieldData $ f where
        cvt _ (SQLite.SQLInteger 0) = Ok Tell
        cvt _ (SQLite.SQLInteger 1) = Ok Ask
        cvt f _                     = returnError ConversionFailed f "Expecting SQLInteger type"
instance ToField StoredMessageType where
    toField Tell = SQLite.SQLInteger 0
    toField Ask  = SQLite.SQLInteger 1

instance SQLite.FromRow StoredMessage where
    fromRow = StoredMessage
        <$> SQLite.field -- mHost
        <*> SQLite.field -- mChannel
        <*> SQLite.field -- mSender
        <*> SQLite.field -- mRecipient
        <*> SQLite.field -- mTime
        <*> SQLite.field -- mMsg
        <*> SQLite.field -- mType
instance SQLite.ToRow   StoredMessage where
    toRow m =
        [ SQLite.SQLText    ( mHost m      )
        , SQLite.SQLText    ( mChannel m   )
        , SQLite.SQLText    ( mSender m    )
        , SQLite.SQLText    ( mRecipient m )
        , toField           ( mTime m )
        , SQLite.SQLText    ( mMsg m       )
        , toField           ( mType m      )
        ]

initialize :: Irc ()
initialize = do
    withDatabase $ \db -> SQLite.execute_ db
        "CREATE TABLE IF NOT EXISTS messages ( \
        \   id          INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \
        \   host        TEXT                              NOT NULL, \
        \   channel     TEXT                              NOT NULL, \
        \   sender      TEXT                              NOT NULL, \
        \   recipient   TEXT                              NOT NULL, \
        \   time        TEXT                              NOT NULL, \
        \   message     TEXT                              NOT NULL, \
        \   type        INTEGER                           NOT NULL  \
        \)"
    withDatabase $ \db -> SQLite.execute_ db
        "CREATE TABLE IF NOT EXISTS notifies ( \
        \   id  INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \
        \   nick TEXT NOT NULL, \
        \   last_notified TEXT NOT NULL \
        \)"

storeHook :: () -> Irc ()
storeHook _ = onMsgCommands ["^tell", "^ask"] $ do
    host    <- getHost
    channel <- getChannel
    sender  <- getSender
    time    <- liftIO getCurrentTime
    cmd     <- getMsgCommand
    params  <- getMsgCommandText
    let (recipient, message) = breakWord params
    if recipient ==? sender
        then 
            noticeReply =<< liftIO randomError
        else do
            let msg = StoredMessage host channel sender (T.toLower recipient) time message $ t cmd
            withDatabase $ \db -> SQLite.execute db
                "INSERT INTO messages (host, channel, sender, recipient, time, message, type) \
                \VALUES (?, ?, ?, ?, ?, ? ,?)"
                msg
            noticeReply $ recipient <> " will be notified when they next become active."
  where breakWord = second (T.drop 1) . T.break isSpace
        t "^ask" = Ask
        t "^tell" = Tell

activityHook :: () -> Irc ()
activityHook _ = onCommands ["PRIVMSG", "JOIN", "AWAY"] $ do
    cmd     <- getCommand
    sender  <- getSender
    case cmd of
        "PRIVMSG" -> do
            channel   <- getChannel
            msgs      <- checkMessages sender $ Just channel
            unless (null msgs) $ notifyUserIn sender channel msgs
        "JOIN"    -> do
            params    <- getParameters
            let channel = head params
            msgs      <- checkMessages sender $ Just channel
            unless (null msgs) $ notifyUserIn sender channel msgs
        "AWAY"    -> do
            params    <- getParameters
            case params of
                [""] -> do
                    msgs <- checkMessages sender Nothing
                    unless (null msgs) $ notifyUserIn sender sender msgs
                _ -> return ()

checkHook :: () -> Irc ()
checkHook _ = onMsgCommands ["^messages", "^msgs", "^messages!", "^msgs!"] $ do
    cmd    <- getMsgCommand
    c      <- getChannel
    sender <- getSender
    msgs <- case T.last cmd of
        '!' -> loadMessages sender Nothing
        _   -> loadMessages sender $ Just c

    forM_ msgs $ \m -> do
        noticeReply $ mSender m <> s (mType m) <> mMsg m
      where s Ask = " asked: "
            s Tell = " wrote: "

loadMessages :: Text -> Maybe Text -> Irc [StoredMessage]
loadMessages recipient (Just channel) = do
    messages <- withDatabase $ \db -> SQLite.query db
        "SELECT host, channel, sender, recipient, time, message, type FROM messages \
        \WHERE recipient = ? AND channel = ? \
        \ORDER BY id"
        (recipient', channel) :: IO [StoredMessage]

    withDatabase $ \db -> SQLite.execute db
        "DELETE FROM messages \
        \WHERE recipient = ? AND channel = ?"
        (recipient', channel)

    return messages
  where recipient' = T.toLower recipient
loadMessages recipient Nothing = do
    messages <- withDatabase $ \db -> SQLite.query db
        "SELECT host, channel, sender, recipient, time, message, type FROM messages \
        \WHERE recipient = ? \
        \ORDER BY id"
        (SQLite.Only recipient') :: IO [StoredMessage]

    withDatabase $ \db -> SQLite.execute db
        "DELETE FROM messages \
        \WHERE recipient = ?"
        (SQLite.Only recipient')

    return messages
  where recipient' = T.toLower recipient

checkMessages :: Text -> Maybe Text -> Irc [StoredMessage]
checkMessages recipient (Just channel) = do
    messages <- withDatabase $ \db -> SQLite.query db
        "SELECT host, channel, sender, recipient, time, message, type FROM messages \
        \WHERE recipient = ? AND channel = ? \
        \ORDER BY id"
        (recipient', channel) :: IO [StoredMessage]
    return messages
  where recipient' = T.toLower recipient
checkMessages recipient Nothing = do
    messages <- withDatabase $ \db -> SQLite.query db
        "SELECT host, channel, sender, recipient, time, message, type FROM messages \
        \WHERE recipient = ? \
        \ORDER BY id"
        (SQLite.Only recipient') :: IO [StoredMessage]
    return messages
  where recipient' = T.toLower recipient

notifyUserIn :: Text -> Text -> [StoredMessage] -> Irc ()
notifyUserIn user chan msgs =
    noticeChannel chan $ user <> ", you have " <> T.pack (show $ length msgs) <> " unread Messages."
