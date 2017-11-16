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
    print config
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

    sess <- setupSession config

    fire <- setupAll sess

    let gnuts = GnutS (Map.fromList []) fire sess (\_ -> return ())

    runGnut config gnuts eventLoop
