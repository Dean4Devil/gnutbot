module Gnut.Plugin.Sed
    ( plugin
    ) where

import              Control.Monad.Trans     (liftIO)
import              Control.Monad           (forM_, when)
import              Data.Array
import              Data.List               (delete)
import              Data.Text               (Text)
import              Data.Maybe              (fromJust)
import qualified    Data.Text               as T
import qualified    Data.Attoparsec.Text    as A
import qualified    Database.SQLite.Simple  as SQLite
import              Text.Regex.TDFA

import Gnut.IRC
import Gnut.Message
import Gnut.MsgCommands

plugin :: NotYetPlugin
plugin = makePlugin "Sed" [sedHook]

sedHook :: Irc ()
sedHook = onCommand "PRIVMSG" $ do
    channel <- getChannel
    msg'    <- getMessageText
    nick    <- getSender
    let msg = T.unpack msg'

    case A.parseOnly msgParser msg' of
        -- Its not a sed pattern so just stop execution.
        Left error -> return ()
        Right (pattern, replace, Just options) -> do
            logMessages <- query options
            searchReplace pattern replace logMessages options
        Right (pattern, replace, Nothing) -> do
            logMessages <- query []
            searchReplace pattern replace logMessages []

query :: [Char] -> Irc [(Text, String)]
query options = do
    channel <- getChannel
    nick    <- getSender
    if 'o' `elem` options then
        withDatabase $ \db -> SQLite.query db
            "SELECT nick, message FROM channellog \
            \WHERE channel = ? and nick = ?\
            \ORDER BY id DESC \
            \LIMIT 100"
            (channel, T.unpack nick) :: IO [(Text, String)]
    else
        withDatabase $ \db -> SQLite.query db
            "SELECT nick, message FROM channellog \
            \WHERE channel = ? \
            \ORDER BY id DESC \
            \LIMIT 100"
            (SQLite.Only channel) :: IO [(Text, String)]


searchReplace :: Text -> Text -> [(Text, String)] -> [Char] -> Irc ()
searchReplace pattern replace messages options = do
    nick    <- getSender
    msg'    <- getMessageText
    let msg =  T.unpack msg'
    case messages of
        [] -> return ()
        ls -> do
            let ls' = delete (nick, msg) ls
            let regex = makeRegexOpts compOpts execOpts $ T.unpack pattern :: Regex
            case firstMatch regex ls' of
                Just (n,m) -> do
                    let array = fromJust $ matchOnce regex m
                    let (b,e) = array ! 0
                    let (x,xs) = splitAt b m
                    let (_,y) = splitAt e xs
                    let fs = x ++ T.unpack replace ++ y
                    noticeReply $ "<" <> n <> "> " <> (T.pack fs)
                Nothing -> return ()
  where
    compOpts = CompOption { caseSensitive = ('i' `notElem` options)
                          , multiline = ('m' `elem` options)
                          , rightAssoc = True
                          , newSyntax = True
                          , lastStarGreedy = False}
    execOpts = ExecOption { captureGroups = ('c' `elem` options) }

firstMatch :: Regex -> [(Text, String)] -> Maybe (Text, String)
firstMatch _ [] = Nothing
firstMatch regex ((n,m):xs) =
    if matchTest regex m :: Bool
    then Just (n,m)
    else firstMatch regex xs

msgParser :: A.Parser (Text, Text, Maybe [Char])
msgParser = do
    A.char 's'
    delim         <- A.satisfy isDelim
    -- Empty pattern makes *no* sense.
    pattern       <- A.takeWhile1 (/= delim)
    A.char delim
    -- Empty replace however does.
    replace       <- A.takeWhile (/= delim)
    A.char delim
    options       <- A.takeText
    if T.null options then
        return (pattern, replace, Nothing)
    else
        return (pattern, replace, Just $ T.unpack options)

isDelim :: Char -> Bool
isDelim = flip elem ("/:" :: String)
