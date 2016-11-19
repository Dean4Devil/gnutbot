module Gnut.Plugin
    ( defplugins
    ) where

import Gnut.IRC
import qualified Gnut.Plugin.Join
import qualified Gnut.Plugin.Ping
import qualified Gnut.Plugin.ServerHello
import qualified Gnut.Plugin.Helo
import qualified Gnut.Plugin.NumberConvert
import qualified Gnut.Plugin.MessageStore
import qualified Gnut.Plugin.Error
import qualified Gnut.Plugin.ChanLog
import qualified Gnut.Plugin.Sed
import qualified Gnut.Plugin.Perlis

defplugins :: [NotYetPlugin]
defplugins =
    [ Gnut.Plugin.Join.plugin
    , Gnut.Plugin.Ping.plugin
    , Gnut.Plugin.ServerHello.plugin
    , Gnut.Plugin.Helo.plugin
    , Gnut.Plugin.NumberConvert.plugin
    , Gnut.Plugin.MessageStore.plugin
    , Gnut.Plugin.Error.plugin
    , Gnut.Plugin.ChanLog.plugin
    , Gnut.Plugin.Sed.plugin
    , Gnut.Plugin.Perlis.plugin
    ]
