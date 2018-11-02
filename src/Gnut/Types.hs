{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Gnut.Types where

import Network.Xmpp.Internal

class GnutModule m where
    --startupModule :: IO m
    stanzaFilter :: m -> Stanza -> Bool
    handleStanza :: m -> Stanza -> IO ()

class GnutIMModule m where
    --startupIMModule :: IO m
    messageFilter :: m -> InstantMessage -> Bool
    handleMessage :: m -> InstantMessage -> IO ()

instance GnutIMModule a => GnutModule a where
    --startupModule = startupIMModule

    stanzaFilter mod (MessageS m) = case getIM m of
        Nothing -> False
        Just m -> messageFilter mod m

    handleStanza mod (MessageS m) = case getIM m of
        Nothing -> return ()
        Just m -> handleMessage mod m
