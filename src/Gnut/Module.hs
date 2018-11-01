module Gnut.Module where

import Gnut.Types

import Data.Map (Map)
import qualified Data.Map as M

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Gnut.Modules.Echo as Echo

type Modules = Map String (Message -> IO ())
type ModuleStore = Behavior (Message -> IO ())

register :: String -> (Message -> IO ()) -> Modules -> Modules
register = M.insert

unregister :: String -> Modules -> Modules
unregister = M.delete

runMod :: Modules -> Message -> IO ()
runMod mods m = do
    let c = content m
        a = M.lookup c mods
    case a of
        Just f -> f m
        Nothing -> return ()

pureMods :: ModuleStore
pureMods = runMod <$> pure m
  where m = M.fromList [("hai", \_ -> putStrLn "Hello there"), Echo.plugin]
