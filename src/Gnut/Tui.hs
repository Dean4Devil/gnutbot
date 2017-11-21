module Gnut.Tui
    ( tuiNetwork
    )
    where

import Gnut.Xmpp
import Gnut.DynamicLoad

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

tuiNetwork :: ModuleStore -> AddHandler LoadEvent -> AddHandler Text -> MomentIO ()
tuiNetwork commands loaderI input = do
    (commandB, commandH) <- newBehavior commands

    inputE <- fromAddHandler input
    loaderE <- fromAddHandler loaderI

    let loadB = fmap loadModule commandB
        unloadB = fmap unloadModule commandB

        (loadE, unloadE) = split loaderE
        load = loadB <@> loadE
        unload = unloadB <@> unloadE

        cmdLookup = fmap queryStore commandB

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
