{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment.XDG.BaseDir
import System.FilePath.Posix
import System.Directory

import Gnut
import System.Log.Logger

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe

import qualified Data.Map as Map

import Data.Text (Text)

main = do

    configDirectory <- getUserConfigDir "gnut"

    config <- fromJust <$> parseConfig (configDirectory ++ "/config.yml")

    run config
