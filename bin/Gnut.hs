{-# LANGUAGE OverloadedStrings #-}
module Main where

import Gnut
import System.Log.Logger

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe

import qualified Data.Map as Map

import Data.Text (Text)

main = do
    config <- fromJust <$> parseConfig "/home/dqubed/.config/gnut/config.yml"
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    sess <- setupSession config

    tuiLoop sess
