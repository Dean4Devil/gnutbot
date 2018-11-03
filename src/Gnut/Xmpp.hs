module Gnut.Xmpp
    ( setupSession
    , teardownSession
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Default
import Network.Socket (HostName)

import Network.Xmpp
import Network.Xmpp.IM
import Network.Xmpp.Internal (TlsBehaviour(..))

setupSession :: HostName -> AuthData -> IO Session
setupSession domain authdata = do
    result <- session domain authdata $ def
        & streamConfigurationL .  tlsBehaviourL .~ RequireTls
        & onConnectionClosedL .~ reconnectSession
    sess <- case result of
            Right s -> return s
            Left e -> liftIO $ error $ "XMPP Failure: " ++ show e
    sendPresence presenceOnline sess
    return sess
  where
    reconnectSession sess failure = void (reconnect' sess)

teardownSession :: Session -> IO ()
teardownSession session = do
    sendPresence presenceOffline session
    endSession session
