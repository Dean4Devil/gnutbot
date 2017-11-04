{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Control.Event.Handler

import Data.Maybe

import Control.Monad
import Data.Default
import Lens.Family2
import Network.Xmpp
import Network.Xmpp.IM
import Network.Xmpp.Internal (TlsBehaviour(..))
import System.Log.Logger

import Data.Text (Text)

main = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    result <-
        session "paranoidlabs.org" (Just (const [plain "gnut" Nothing "quailaeQu3ahbei0vaXa"], Nothing)) $ def &
        streamConfigurationL .
        tlsBehaviourL .~
        RequireTls &
        onConnectionClosedL .~
        reconnectSession
    sess <-
        case result of
            Right s -> return s
            Left e -> error $ "XMPP Failure: " ++ show e
    sendPresence presenceOnline sess

    (addHandler, fire) <- newAddHandler
    network <- setupNetwork sess addHandler

    actuate network

    forever $ do
        msg <- getMessage sess
        fire msg
  where
    reconnectSession sess failure = void (reconnect' sess)

data Command = Hai | HowDoing | InvalidCommand

setupNetwork :: Session -> AddHandler Message -> IO EventNetwork
setupNetwork sess esmsg = compile $ do
    emsg <- fromAddHandler esmsg

    reactimate $ fmap print emsg
    reactimate $ fmap (evalMessage sess) emsg

evalMessage :: Session -> Message -> IO ()
evalMessage s m = do
    let m' = getIM m
    case m' of
        Nothing -> return ()
        Just im -> do
            let c = parseCommand . showMessage $ im
            evalCommand s (m, c)

evalCommand :: Session -> (Message, Command) -> IO ()
evalCommand sess (m, Hai) = 
        void (sendMessage (fromJust (answerIM [MessageBody Nothing "Hai!" ] m)) sess)
evalCommand sess (m, HowDoing) =
        void (sendMessage (fromJust (answerIM [MessageBody Nothing "Baby steps...." ] m)) sess)
evalCommand sess (m, InvalidCommand) =
        void (sendMessage (fromJust (answerIM [MessageBody Nothing "I don't know that command, sorry."] m)) sess)

parseCommand :: Text -> Command
parseCommand "^Hai" = Hai
parseCommand "^HowsStuff" = HowDoing
parseCommand _ = InvalidCommand

showMessage :: InstantMessage -> Text
showMessage msg = do
    let bodies = imBody msg
    let contents = map bodyContent bodies
    head contents

mcons :: Monad m => m a -> m b -> m (a,b)
mcons x y = do
    a <- x
    b <- y
    return (a,b)
