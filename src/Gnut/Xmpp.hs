module Gnut.Xmpp
    ( setupSession
    , getMessage
    , Gnut.Xmpp.sendMessage
    , xmppLoop
    )
    where

import Gnut.Config
import qualified Gnut.Types as G

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Default

import qualified Data.Text as T

import Reactive.Banana.Frameworks

import Network.Xmpp
import Network.Xmpp.IM
import Network.Xmpp.Internal (TlsBehaviour(..))

setupSession :: Config -> IO Session
setupSession c = do
    result <- session ("paranoidlabs.org") (Just (const [plain "gnut" Nothing "quailaeQu3ahbei0vaXa"], Nothing)) $ def
        & streamConfigurationL .  tlsBehaviourL .~ RequireTls
        & onConnectionClosedL .~ reconnectSession
    sess <- case result of
            Right s -> return s
            Left e -> liftIO $ error $ "XMPP Failure: " ++ show e
    sendPresence presenceOnline sess
    return sess
  where
    reconnectSession sess failure = void (reconnect' sess)

sendMessage :: Message -> G.Gnut ()
sendMessage m = do
    s <- G.gnutSession <$> G.get
    liftIO $ void $ Network.Xmpp.sendMessage m s

xmppLoop :: Session -> Handler Message -> IO ()
xmppLoop sess sink = loop
    where
    loop = do
        m <- getMessage sess
        _ <- sink m
        loop
