module Gnut.EventNetwork
    ( routerNetwork
    )
    where

import Gnut.Types
import Gnut.Xmpp

import Data.Set (Set)
import qualified Data.Set as Set

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Set of ignored JIDs. If the Sender is in the Set all events are directly
-- ignored
ignoreF :: Set Jid -> Message -> Maybe Message
ignoreF s m@(Message _ (Just jid) _ _ _ [] []) = if jid `Set.member` s then Nothing else Just m
ignoreF s m@(Message _ Nothing _ _ _ [] []) = Just m

-- Ignore / Unignore
type IgnoreEvent = Either Jid Jid

routerNetwork :: Set Jid -> AddHandler IgnoreEvent -> AddHandler Message -> MomentIO ()
routerNetwork ignores ignoreI input = do
    (ignoreSet, ignoreH) <- newBehavior ignores

    inputE <- fromAddHandler input
    changeIgnoreE <- fromAddHandler ignoreI

    let ignoreB = fmap ignoreUser ignoreSet
        unignoreB = fmap unignoreUser ignoreSet

        (ignoreE, unignoreE) = split changeIgnoreE
        ignore = ignoreB <@> ignoreE
        unignore = unignoreB <@> unignoreE

        passed = 

    reactimate $ ignoreH <@> ignore
    reactimate $ ignoreH <@> unignore

ignoreUser :: Set Jid -> Jid -> Set Jid
ignoreUser s k = Set.insert k s

unignoreUser :: Set Jid -> Jid -> Set Jid
unignoreUser s k = Set.delete k s
