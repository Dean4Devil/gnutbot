module Gnut.DynamicLoad
    ( ModuleStore
    , LoadEvent
    , queryStore
    , loadModule
    , unloadModule
    )
    where

import Data.Either

import Data.Text (Text)

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map

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
