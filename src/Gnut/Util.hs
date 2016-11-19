module Gnut.Util
    ( randomElement
    ) where

import System.Random (randomRIO)

randomElement :: [a] -> IO a
randomElement xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
