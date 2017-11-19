module Gnut.Tui
    ( tuiLoop
    )
    where

import Prelude hiding (getLine, putStrLn)

import Gnut.Types

import qualified Data.Text
import Data.Text (Text)
import Data.Text.IO (getLine, putStrLn)

import System.IO hiding (getLine, putStrLn)

import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

printUsage :: Gnut ()
printUsage = liftIO $ mapM_ putStrLn
    [ "Gnut Terminal User interface"
    , "Throw lines at it and see if they work."
    ]

tuiLoop :: IO ()
tuiLoop = loop
    where
    loop = do
        putStr "λ "
        hFlush stdout
        l <- getLine
        putStrLn l
        loop
