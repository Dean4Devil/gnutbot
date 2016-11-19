{-# LANGUAGE OverloadedStrings #-}
module Gnut 
    ( startup
    , startupWith
    ) where

import   Control.Applicative       ((<$>))
import   Control.Concurrent        (forkIO)
import   Control.Concurrent.MVar   (newMVar)
import   Control.Monad             (forM, forM_)
import   Data.Maybe                (catMaybes)
import   qualified                 Data.Text as T
import   qualified                 Database.SQLite.Simple as SQLite
import   System.Environment        (getProgName)

import Gnut.App
import Gnut.Logger
import Gnut.IRC
import Gnut.Plugin

-- Start Gnut with default plugins
startup :: IrcConfig -> IO ()
startup = startupWith defplugins

-- Start Gnut with a defined set of handlers
startupWith :: [NotYetPlugin] -> IrcConfig -> IO ()
startupWith plugins config = do
    logName <- (++ ".log") <$> getProgName
    logger <- makeLogger logName

    runApp logger (T.unpack $ ircHost config) (ircPort config) $
        app logger plugins config

app :: Logger -> [NotYetPlugin] -> IrcConfig -> App
app logger plugins config writer = do
    db <- newMVar =<< SQLite.open (ircDatabase config)
    let env = IrcEnvironment config db writer logger

    plugins' <- forM plugins $
        \p -> do
            let state = IrcState env
                    (error "Message not known yet")
                    (error "Uninitialized plugin")
            r <- initializePlugin p state
            {-
             -case r of
             -    Nothing -> logger $ "Could not initialize plugin: " <> name
             -    Just _  -> logger $ "Initialized plugin: " <> name
             -}
            return r

    return $ \msg -> 
        forM_ plugins' $ \p -> do
            let state = IrcState env msg p
            _ <- forkIO $ runPlugin p state
            return ()
