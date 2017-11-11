module Gnut.Command.Xmpp
    ( parseMessage
    , parseMessageB
    )
    where

import Gnut.Types

import Data.Maybe

import Network.Xmpp
import Network.Xmpp.IM

import Reactive.Banana

parseMessageB :: Behavior (Message -> Maybe Command)
parseMessageB = pure parseMessage

parseMessage :: Message -> Maybe Command
parseMessage m = do
    im <- getIM m
    let t = bodyContent . head . imBody $ im
    Just (t, m)
