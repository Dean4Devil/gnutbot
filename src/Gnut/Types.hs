module Gnut.Types
    ( Gnut
    , MUCName
    , GnutS(..)
    , Command
    , runGnut
    , ask
    , local
    , get
    , put
    )
    where

import Gnut.Config

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.IO.Class

import Reactive.Banana.Frameworks

import Network.Xmpp

import qualified Data.Map as Map

import Data.Text (Text)

type Gnut a = StateT GnutS (R.ReaderT Config IO) a

runGnut :: Config -> GnutS -> Gnut a -> IO (a, GnutS)
runGnut c s g = R.runReaderT (runStateT g s) c

-- Lift ReaderT up
ask :: Gnut Config
ask = lift R.ask

local :: (Config -> Config) -> Gnut a -> Gnut a
local f = mapStateT (R.local f)

type MUCName = Text

data GnutS = GnutS
    { channelmap :: Map.Map MUCName (Command -> IO ())
    , globalHndl :: Handler Message
    , gnutSession :: Session
    , plugins :: Message -> Gnut ()
    }

type Command = (Text, Message)

