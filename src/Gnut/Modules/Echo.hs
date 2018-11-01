module Gnut.Modules.Echo
    ( plugin )
    where

import Gnut.Types

plugin :: (String, (Message -> IO ()))
plugin = ("echo", print)
