module Gnut.Permissions
    ( Permissions
    , PermBehavior
    , checkPermission
    , purePerm
    )
    where

import Gnut.Types
import Gnut.Module

import Reactive.Banana
import Reactive.Banana.Frameworks

data Permissions = Permissions { admin :: String }

type PermBehavior = Behavior (Message -> Bool)

checkPermission :: Permissions -> Message -> Bool
checkPermission store message = do
    jid message == admin store

purePerm :: PermBehavior
purePerm = checkPermission <$> pure p
  where p = Permissions { admin = "dean" }
