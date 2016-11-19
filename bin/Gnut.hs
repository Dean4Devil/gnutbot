{-# LANGUAGE OverloadedStrings #-}
module Main where

import Gnut
import Gnut.IRC

main :: IO ()
main = startup IrcConfig
    { ircNick = "Gnut"
    , ircRealName = "Gnutbot"
    , ircChannels = ["#Paranoidlabs"]
    , ircHost = "chat.freenode.net"
    , ircPort = 6667
    , ircDatabase = "test.db"
    , ircNickServ = Nothing
    }
