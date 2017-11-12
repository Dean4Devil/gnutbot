module Gnut.Tui
    ( tuiLoop
    , printUsage
    )
    where

import Prelude hiding (getLine, putStrLn, putStr, words)

import Gnut.Types
import Gnut.Command.Tui

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO (getLine, putStrLn, putStr)

import qualified System.IO as SIO

import Reactive.Banana
import Reactive.Banana.Frameworks

printUsage :: Gnut ()
printUsage = liftIO $ mapM_ putStrLn
    [ "Gnut Terminal User interface"
    , "Throw lines at it and see if they work."
    ]

tuiLoop :: Gnut ()
tuiLoop = printUsage >>= loop
    where
    loop = do
        l <- liftIO $ do
            putStr "λ "
            SIO.hFlush SIO.stdout
            getLine
        executeCommand $ parseCommand l
        loop

parseCommand :: Text -> (Text, [Text])
parseCommand "" = ("", [])
parseCommand t = (x, xs)
    where x:xs = T.words t
