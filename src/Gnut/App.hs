{-# LANGUAGE OverloadedStrings #-}
module Gnut.App
    ( App
    , runApp
    ) where

import   qualified   Data.Text             as T
import   qualified   Data.Text.Encoding    as T
import   qualified   Data.ByteString.Char8 as B

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)

import Network.Connection
import Network.Socket (PortNumber)

import Gnut.IRC
import Gnut.Logger
import Gnut.Message
import Gnut.Message.Encoder
import Gnut.Message.Decoder

type App = (Message -> IO ()) -> IO (Message -> IO ())

runApp :: Logger -> String -> PortNumber -> App -> IO ()
runApp logger host port app = do
    conn <- connect'
    writer <- makeMessageWriter logger conn
    app' <- app writer
    go conn app'
  where
    connect' = do
        context <- initConnectionContext
        connectTo context $ ConnectionParams host port Nothing Nothing

    go conn app' = do
        msg <- readMessage logger conn
        case msg of
            Nothing            -> return ()
            Just msg -> app' msg >> go conn app'

readMessage :: Logger -> Connection -> IO (Maybe Message)
readMessage logger conn = do
    line <- connectionGetLine 1024 conn
    if B.null line then
        return Nothing
    else
        case decode line of
            Just msg -> do
                logger $ T.pack $ ">> " ++ show msg ++ "\n"
                return $ Just msg
            Nothing -> do
                logger $ "Gnut.App.readMessage: Can't parse: " `T.append` T.decodeUtf8 line
                readMessage logger conn

makeMessageWriter :: Logger -> Connection -> IO (Message -> IO ())
makeMessageWriter logger conn = do
    lineWriter <- makeLineWriter conn
    return $ \cmd -> do
        let bs = encode cmd
        logger $ T.pack $ "<< " ++ show cmd ++ "\n"
        lineWriter bs

makeLineWriter :: Connection -> IO (B.ByteString -> IO ())
makeLineWriter conn = do
    lock <- newMVar ()
    return $ \bs -> bs `seq` do
        () <- takeMVar lock
        connectionPut conn $ B.append bs "\r\n"
        putMVar lock ()
