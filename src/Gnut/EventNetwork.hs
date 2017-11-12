module Gnut.EventNetwork
    ( 
    )
    where

import Gnut.Types

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import Network.Xmpp

import Data.Text (Text)
import qualified Data.Text as T

data GnutEvent = Message | (Text, [Text])

globalEventNetwork :: AddHandler Message -> AddHandler (Text, [Text]) -> MomentIO
globalEventNetwork esxmpp esstdin = do
    exmpp <- fromAddHandler esxmpp
    estdin <- fromAddHandler estdin

    let eex = apply (pure $ GnutEvent) exmpp
        ees = apply (pure $ GnutEvent) estdin

        evs = unionWith (\x,y -> x) ees eex

    
