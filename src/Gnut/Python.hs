module Gnut.Python
    where

import System.IO (stdout)

import qualified CPython as Py
import qualified CPython.System as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Integer as Py


setupPy :: IO ()
setupPy = do
    Py.initialize
    Py.setPath "/tmp/gnut"

runPy :: IO ()
runPy = do
    setupPy
    m <- Py.importModule "time"
    f <- Py.getAttribute m =<< Py.toUnicode "sleep"
    a <- mapM Py.toInteger ([5] :: [Integer])
    r <- Py.callArgs f $ map Py.toObject a
    Py.print r stdout
