module Gnut.Modules.Hello
    ( Hello(..)
    , messageFilter
    , handleMessage
    )
    where

import System.IO

import Network.Xmpp.IM

import Gnut.Types

data Hello = Hello

instance GnutIMModule Hello where
    messageFilter _ m = True

    handleMessage _ m = print m
