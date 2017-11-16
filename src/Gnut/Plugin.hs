module Gnut.Plugin
    ( Plugin(..)
    )
    where

import Gnut.Types

data Plugin = Plugin
    { pFilter :: Message -> Bool
    , pBody :: Message -> Gnut ()
    }
