module Gnut.Tui
    ( tuiLoop
    )
    where

import Gnut.Xmpp

import Prelude hiding (putStr, putStrLn, getLine)

import Control.Concurrent.MVar

import Control.Monad (when)

import Data.Maybe
import Data.Either

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map

import Data.Text (Text)
import qualified Data.Text as T

import Data.Text.IO (putStr, putStrLn, getLine)
import System.IO hiding (putStr, putStrLn, getLine)

import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

tuiNetwork :: TuiCommands -> AddHandler LoadEvent -> AddHandler Text -> MomentIO ()
tuiNetwork commands loaderI input = do
    (commandB, commandH) <- newBehavior commands

    inputE <- fromAddHandler input
    loaderE <- fromAddHandler loaderI

    let loadB = fmap loadMod commandB
        unloadB = fmap unloadMod commandB

        (loadE, unloadE) = split loaderE
        load = loadB <@> loadE
        unload = unloadB <@> unloadE

        cmdLookup = fmap lookupM commandB

        -- TODO split Either Line NoLine
        cmdLine = filterE (/= "") inputE
        emptyLine = filterE (== "") inputE

        -- Split the line into words and try to resolve the first one into
        -- a command
        cmdM = apply cmdLookup $ pure T.words <@> cmdLine

        -- Command is either Invalid or valid.
        -- TODO split Either Command NoCommand
        cmdNF = filterE isNothing cmdM
        cmdIO = filterJust cmdM

    reactimate $ putStrLn "" <$ emptyLine
    reactimate $ putStrLn "Invalid command: " <$ cmdNF

    -- If command exists, run command
    reactimate cmdIO

    reactimate $ commandH <$> load
    reactimate $ commandH <$> unload

tuiLoop :: Session -> IO ()
tuiLoop sess = do
    (lineH, send) <- newAddHandler
    (loadH, load) <- newAddHandler

    let avail = Map.singleton "echo" print

    let cmds = Map.fromList [ ("send", sendM sess)
                            , ("quit", \_ -> return ())
                            , ("load", cmdLoadModule avail load)
                            , ("unload", cmdUnloadModule load)
                            ]


    network <- compile $ tuiNetwork cmds loadH lineH
    actuate network

    loopdiloop send network

loopdiloop :: Handler Text -> EventNetwork -> IO ()
loopdiloop fire network = loop
  where
    loop = do
        putStr "> "
        hFlush stdout
        hSetBuffering stdin NoBuffering
        l <- getLine
        fire l
        when (l /= "quit") loop

sendM :: Session -> [Text] -> IO ()
sendM sess [] = putStrLn "send <jid> <message>"
sendM sess [x] = putStrLn "send <jid> <message>"
sendM sess (jid:text) = case jidFromText jid of
    Nothing -> putStrLn "Invalid JID"
    Just j -> do
        r <- sendMessage (simpleIM j (T.unwords text)) sess
        return ()

cmdLoadModule :: TuiCommands -> Handler LoadEvent -> [Text] -> IO ()
cmdLoadModule m load [k] = do
    let mod = Map.lookup k m
    case mod of
        Just mod' -> load $ Left (k, mod')
        Nothing -> putStrLn $ T.append "No such module: " k
cmdLoadModule m load [] = putStrLn "load <modulename>"
cmdLoadModule m load _ = putStrLn "Module options are not supported yet, sorry."

cmdUnloadModule :: Handler LoadEvent -> [Text] -> IO ()
cmdUnloadModule load [k] = load $ Right k
cmdUnloadModule load _ = putStrLn "unload <modulename>"
