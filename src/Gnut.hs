module Gnut (run) where

import System.IO
import Control.Monad (when)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Text (Text)

import Gnut.Router

run :: IO ()
run = do
    msgsrc <- newAddHandler
    network <- setupRouterNetwork purePerm pureMods (addHandler msgsrc)
    actuate network
    eventLoop msgsrc

eventLoop :: (EventSource Message) -> IO ()
eventLoop esmsg = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        s <- getLine
        if s == "" then do
            putStrLn "Commands: d <msg>|o <msg>|quit"
            loop
        else do
            let (x:xs) = words s
            case x of
                "d" -> fire esmsg $ message "dean" $ unwords xs
                "o" -> fire esmsg $ message "other" $ unwords xs
                "quit" -> return ()
                _ -> putStrLn "Commands: d <msg>|o <msg>|quit"
            when (x /= "quit") loop

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd
