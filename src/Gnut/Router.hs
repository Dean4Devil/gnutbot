module Gnut.Router
    ( setupRouterNetwork
    , purePerm
    , pureMods

    , Message
    , message
    )
    where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Gnut.Types
import Gnut.Module
import Gnut.Permissions

setupRouterNetwork :: PermBehavior -> ModuleStore -> (AddHandler Message) -> IO EventNetwork
setupRouterNetwork ignored modules esmsg = compile $ do
    emsg <- fromAddHandler esmsg
    let eok = filterApply ignored emsg
        handler = modules <@> eok

    reactimate $ fmap id handler
