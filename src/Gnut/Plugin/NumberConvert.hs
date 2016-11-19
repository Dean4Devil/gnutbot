module Gnut.Plugin.NumberConvert
    ( plugin
    ) where

import Data.Char (digitToInt, intToDigit)
import Numeric
import Data.List (isPrefixOf)
import qualified Data.Text as T

import Gnut.IRC
import Gnut.MsgCommands

plugin :: NotYetPlugin
plugin = makePlugin "NumberConvert" [hexHook, unhexHook, octHook, binHook, unbinHook]

hexHook :: Irc ()
hexHook = onMsgCommand "^hex" $ do
    n <- read . T.unpack <$> getMsgCommandText
    noticeReply $ T.pack $ "0x" ++ showHex (n :: Integer) ""

unhexHook :: Irc ()
unhexHook = onMsgCommand "^unhex" $ do
    n <- T.unpack <$> getMsgCommandText
    let n' = if "0x" `isPrefixOf` n then n else "0x" ++ n
    noticeReply $ T.pack $ show (read n' :: Integer)

octHook :: Irc ()
octHook = onMsgCommand "^oct" $ do
    n <- read . T.unpack <$> getMsgCommandText
    noticeReply $ T.pack $ "0" ++ showOct (n :: Integer) ""

-- Meh
{-unoctHook :: Irc ()-}
{-unoctHook = onMsgCommand "^unoct" $ do-}
    {-n <- T.unpack <$> getMsgCommandText-}
    {-let n' = if "0" `isPrefixOf` n then n else "0" ++ n-}
    {-noticeReply $ T.pack $ show (read n' :: Integer)-}

binHook :: Irc ()
binHook = onMsgCommand "^bin" $ do
    n <- read . T.unpack <$> getMsgCommandText
    noticeReply $ T.pack $ showIntAtBase 2 intToDigit (n :: Integer) ""

unbinHook :: Irc ()
unbinHook = onMsgCommand "^unbin" $ do
    n <- read . T.unpack <$> getMsgCommandText
    noticeReply $ T.pack $ show $ T.foldl (\x y -> x * 2 + digitToInt y) 0 n
