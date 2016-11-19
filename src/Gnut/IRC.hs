{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gnut.IRC
  ( IrcConfig (..)
  , IrcEnvironment (..)
  , IrcState (..)
  , Plugin
  , NotYetPlugin
  , Irc
  , makePlugin
  , makeInitPlugin
  , initializePlugin
  , runIrc
  , runPlugin
  , getChannel
  , getChannels
  , getCommand
  , getHost
  , getMessage
  , getMessageText
  , getNick
  , getParameters
  , getPluginName
  , getRealName
  , getSender
  , writeMessage
  , writeChannel
  , noticeChannel
  , writeReply
  , noticeReply
  , onCommand
  , onCommands
  , withDatabase
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent (MVar, modifyMVar_, readMVar, withMVar)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Network.Socket (PortNumber)

import qualified Database.SQLite.Simple as SQLite

import Gnut.Message

data IrcConfig = IrcConfig
    { ircNick                        :: Text
    , ircRealName                    :: Text
    , ircChannels                    :: [Text]
    , ircHost                        :: Text
    , ircPort                        :: PortNumber
    , ircDatabase                    :: FilePath
    , ircNickServ                    :: Maybe (Text, Text)
    }

data IrcEnvironment = IrcEnvironment
    { ircConfig                      :: IrcConfig
    , ircDBConnection                :: MVar SQLite.Connection
    , ircWriter                      :: Message -> IO ()
    , ircLogger                      :: Text -> IO ()
    }

data IrcState = IrcState
    { ircEnvironment                 :: IrcEnvironment
    , ircMessage                     :: Message
    , ircPlugin                      :: Plugin
    }

newtype Irc a = Irc { unIrc :: ReaderT IrcState IO a }
    deriving ( Applicative, Monad, Functor, MonadIO
             , MonadReader IrcState
             )

data Plugin = Plugin
    { pluginName  :: Text
    , pluginHooks :: [Irc ()]
    }

data NotYetPlugin = forall a.
    NotYetPlugin Text [a -> Irc ()] (Irc a)

instance Eq NotYetPlugin where
    (NotYetPlugin x _ _) == (NotYetPlugin y _ _) = x == y

runIrc :: Irc a -> IrcState -> IO a
runIrc irc = runReaderT (unIrc irc)

runPlugin :: Plugin -> IrcState -> IO ()
runPlugin plugin = runIrc (sequence_ $ pluginHooks plugin)

makePlugin :: Text -> [Irc ()] -> NotYetPlugin
makePlugin name hooks = makeInitPlugin name (map const hooks) (return ())

-- Plugin with an initalization routine
makeInitPlugin :: Text -> [a -> Irc ()] -> Irc a -> NotYetPlugin
makeInitPlugin = NotYetPlugin

initializePlugin :: NotYetPlugin -> IrcState -> IO Plugin
initializePlugin (NotYetPlugin name hooks ini) state = do
    x <- runIrc ini state
    return $ Plugin name $ map ($ x) hooks

getNick :: Irc Text
getNick = ircNick . ircConfig . ircEnvironment <$> ask

getRealName :: Irc Text
getRealName = ircRealName . ircConfig . ircEnvironment <$> ask

getHost :: Irc Text
getHost = ircHost . ircConfig . ircEnvironment <$> ask

getChannels :: Irc [Text]
getChannels = ircChannels . ircConfig . ircEnvironment <$> ask

getMessage :: Irc Message
getMessage = ircMessage <$> ask

getPrefix :: Irc Prefix
getPrefix = do
    Just prefix <- messagePrefix . ircMessage <$> ask
    return prefix

getCommand :: Irc Text
getCommand = do
    command <- messageCommand . ircMessage <$> ask
    return command

getParameters :: Irc [Text]
getParameters = do
    param <- messageParameters . ircMessage <$> ask
    return param

getSender :: Irc Text
getSender = do
    prefix <- messagePrefix . ircMessage <$> ask
    return $ case prefix of
        Nothing -> error "No Sender"
        Just (ServerPrefix s)    -> s
        Just (NickPrefix n _ _ ) -> n

getChannel :: Irc Text
getChannel = do
    (channel : _) <- getParameters
    return channel

getMessageText :: Irc Text
getMessageText = do
    params <- getParameters
    return $ case params of
        (_ : t : _) -> t
        _           -> error "No message text"

getPluginName :: Irc Text
getPluginName = pluginName . ircPlugin <$> ask


-- Send a command to the server
writeMessage :: Text -> [Text] -> Irc ()
writeMessage command parameters = do
    writer <- ircWriter . ircEnvironment <$> ask
    liftIO $ writer $ makeMessage command parameters

-- Write a given message to a given channel
writeChannel :: Text -> Text -> Irc ()
writeChannel chan msg =
    writeMessage "PRIVMSG" [chan, msg']
  where
    msg' | T.length msg <= 400 = msg
         | otherwise = T.take 400 msg <> "..."

noticeChannel :: Text -> Text -> Irc ()
noticeChannel chan msg =
    writeMessage "NOTICE" [chan, msg']
  where
    msg' | T.length msg <= 400 = msg
         | otherwise = T.take 400 msg <> "..."

writeReply :: Text -> Irc ()
writeReply reply = do
    source <- getChannel
    writeChannel source reply

noticeReply :: Text -> Irc ()
noticeReply reply = do
    source <- getChannel
    noticeChannel source reply

onCommand :: Text -> Irc () -> Irc ()
onCommand command irc = do
    actual <- getCommand
    when (actual ==? command) irc

onCommands :: [Text] -> Irc () -> Irc ()
onCommands [] _ = return ()
onCommands ls irc = do
    actual <- getCommand
    when (any (==? actual) ls) irc

withDatabase :: (SQLite.Connection -> IO a) -> Irc a
withDatabase f = do
    lock <- ircDBConnection . ircEnvironment <$> ask
    liftIO $ withMVar lock f
