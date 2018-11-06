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
                      -> IO EventNetwork
setupInterfaceNetwork hpmplugin eschan = compile $ do
    echan <- fromAddHandler eschan

    (bchanh, hchanh) <- newBehavior M.empty

    let 
        (eloadchan, eremovechan) = split echan
        eloadchan :: Event (Jid, Handler PlugUpdate)
        eremovechan :: Event Jid

        uloadchan = addChan <$> bchanh <@> eloadchan
        uloadchan :: Event (Map Jid (Handler PlugUpdate))
        uremovechan = delChan <$> bchanh <@> eremovechan
        uremovechan :: Event (Map Jid (Handler PlugUpdate))

    reactimate $ fmap hchanh uloadchan
    reactimate $ fmap hchanh uremovechan

addChan = (flip . uncurry) M.insert
delChan = flip M.delete
