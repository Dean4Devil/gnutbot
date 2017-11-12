module Gnut.Command.Tui
    ( executeCommand
    )
    where

import Gnut.Types
import Gnut.Xmpp

import Control.Monad.IO.Class

import Network.Xmpp hiding (sendMessage)
import qualified Network.Xmpp.IM as IM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

executeCommand :: (Text, [Text]) -> Gnut ()
executeCommand (x, p) =
    case Map.lookup x commandMap of
        Just c -> c p
        Nothing -> liftIO $ putStrLn "Invalid Command!"

commandMap :: Map.Map Text ([Text] -> Gnut ())
commandMap = Map.fromList
    [ ("", \_ -> return ())
    , ("send", sendMsg)
    ]

sendMsg :: [Text] -> Gnut ()
sendMsg [] =
    liftIO $ putStrLn "Refusing to send a message with no recipient."
sendMsg [rec] =
    liftIO $ putStrLn "Refusing to send an empty message."
sendMsg (rec:msg) =
    case jidFromText rec of
        Nothing -> liftIO $ putStrLn "Invalid recipient JID"
        Just j -> sendMessage $ IM.simpleIM j (T.unwords msg)
