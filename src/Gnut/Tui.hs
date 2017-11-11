module Gnut.Tui
    ( 
    )
    where

import Gnut.Types

import qualified Data.Text
import Data.Text (Text, getLine)

import Reactive.Banana

printUsage :: Gnut ()
printUsage = liftIO $ mapM_ putStrLn
    [ "Gnut Terminal User interface"
    , "Throw lines at it and see if they work."
    ]

tuiLoop :: Handler Text -> Gnut ()
tuiLoop sink = loop
    where
    loop = liftIO $ do
        putStrLn "λ "
        hFlush stdout
        l <- getLine

