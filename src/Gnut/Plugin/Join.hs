{-# LANGUAGE OverloadedStrings #-}

module Gnut.Plugin.Join
    ( plugin
    ) where

import Control.Monad (forM_)
import Gnut.IRC

import Data.Text.Encoding

plugin :: NotYetPlugin
plugin = makePlugin "Join" [autoJoinHook]

autoJoinHook :: Irc ()
autoJoinHook = onCommand "376" $ do
    channels <- getChannels
    forM_ channels $ writeMessage "JOIN" . return
