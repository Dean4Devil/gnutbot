module Gnut.DynamicLoad
    ( ModuleStore
    , LoadEvent
    , queryStore
    , loadModule
    , cmdLoadModule
    , unloadModule
    , cmdUnloadModule
    )
    where

import Prelude hiding (putStrLn)

import Data.Either

import Data.Text (Text)
import qualified Data.Text as T

import Data.Text.IO

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map

import Reactive.Banana.Frameworks

type Module = [Text] -> IO ()
type ModName = Text
type ModuleStore = HashMap ModName Module

-- Either (Load Mod) (Unload Name)
type LoadEvent = Either (ModName, Module) ModName

queryStore :: ModuleStore -> [Text] -> Maybe (IO ())
queryStore map [] = Nothing
queryStore map (x:xs) = do
    let f = Map.lookup x map
    fmap ($ xs) f

loadModule :: ModuleStore -> (ModName, Module) -> ModuleStore
loadModule m (k,f) = Map.insert k f m

unloadModule :: ModuleStore -> ModName -> ModuleStore
unloadModule m k = Map.delete k m

cmdLoadModule :: ModuleStore -> Handler LoadEvent -> [Text] -> IO ()
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
