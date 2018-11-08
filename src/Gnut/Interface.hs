module Gnut.Interface
    ( setupInterfaceNetwork
    ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Map (Map)
import qualified Data.Map as M

import Gnut.Types

import Network.Xmpp (Jid)

setupInterfaceNetwork :: Handler PlugUpdate
                      -> AddHandler (Either (Jid, Handler PlugUpdate) Jid)
                      -> AddHandler (Jid, PlugUpdate)
                      -> IO EventNetwork
setupInterfaceNetwork hpmplugin eschan eschanplugin = compile $ do
    echan <- fromAddHandler eschan
    echanplugin <- fromAddHandler eschanplugin

    (bchanh, hchanh) <- newBehavior M.empty

    let 
        (eloadchan, eremovechan) = split echan
        eloadchan :: Event (Jid, Handler PlugUpdate)
        eremovechan :: Event Jid

        uloadchan = addChan <$> bchanh <@> eloadchan
        uloadchan :: Event (Map Jid (Handler PlugUpdate))
        uremovechan = delChan <$> bchanh <@> eremovechan
        uremovechan :: Event (Map Jid (Handler PlugUpdate))

        bchanplugupdate = sendPlugUpdate <$> bchanh
        bchanplugupdate :: Behavior ((Jid, PlugUpdate) -> IO ())
        eupdate = bchanplugupdate <@> echanplugin
        eupdate :: Event (IO ())

    reactimate $ fmap hchanh uloadchan
    reactimate $ fmap hchanh uremovechan

    reactimate eupdate

addChan = (flip . uncurry) M.insert
delChan = flip M.delete

sendPlugUpdate :: Map Jid (Handler PlugUpdate) -> (Jid, PlugUpdate) -> IO ()
sendPlugUpdate m (k, a) = maybe (return ()) (flip ($) a) (M.lookup k m)
