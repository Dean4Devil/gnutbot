module Gnut.NickServ
    ( check
    ) where

import Gnut.Irc
import Gnut.Message



check_admin :: Text -> Irc Maybe Bool
check_admin nick = do
    
