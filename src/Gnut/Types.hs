module Gnut.Types where

import Reactive.Banana
import Reactive.Banana.Frameworks

data Message = Message
             { jid :: String
             , content :: String
             } deriving (Show, Eq, Ord)

message j c = Message {jid=j, content=c}

