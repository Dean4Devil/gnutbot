module Gnut.Types where

import Gnut.Permissions

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

import Network.Xmpp.Internal hiding (Plugin)

import Reactive.Banana.Frameworks

-- Left = Load, Right = Remove
type PlugUpdate = Either (String, Plugin) (String)

type PermUpdate = Either (JidKey, Permission) (JidKey, Permission)

newtype JidKey = JidKey { toJid :: Jid }
    deriving (Eq, Show)

toJidKey :: Jid -> JidKey
toJidKey = JidKey . toBare

data Stuff = Stuff { context :: Context
                   , stanza :: Stanza
                   } deriving (Eq, Show)

data Context = Context { permissions :: [Permissions]
                       , channel :: JidKey
                       } deriving (Eq, Show)

data Channel = Channel
    { network :: EventNetwork -- The EventNetwork the Channel is running
    , shandler :: Handler Stanza -- A Handler to push Stanzas from outside onto the Network
    , mhandler :: Handler PlugUpdate -- Update Plugins inside the network
    , phandler :: Handler PermUpdate -- Update Permissions inside the network
    }
type ChannelMap = Map Jid Channel
data ChannelSettings = ChannelSettings
    { csPlugins :: Map String Plugin
    , csPermissions :: Map Jid [Permissions]
    }
instance Show ChannelSettings where
    show c = "Plugins: " ++ show (M.keys $ csPlugins c) ++ "Permissions: " ++ show (csPermissions c)

data Plugin =
     Plugin { plName :: String
            , plFilter :: Stanza -> Bool
            , plAction :: Stanza -> [Permissions] -> Handler Stanza -> IO ()
            }
instance Show Plugin where
    show = plName

plugin n f a = Plugin { plName = n, plFilter = f, plAction = a }

simplePlugin :: String -> (Stanza -> Bool) -> (Stanza -> [Permissions] -> IO (Maybe Stanza)) -> Plugin
simplePlugin n f a = plugin n f b
  where
    b s p c = do
      r <- a s p
      case r of
          Just r' -> c r'
          Nothing -> return ()

answerPlugin :: String -> (InstantMessage -> Bool) -> (InstantMessage -> [Permissions] -> IO [MessageBody]) -> Plugin
answerPlugin n f a = simplePlugin n g b
  where
    b (MessageS m) p = case getIM m of
        Just im -> do
            mb <- a im p
            case answerIM mb m of
                Just rm -> return $ Just (MessageS rm)
                Nothing -> return Nothing
        Nothing -> return Nothing
    b _ _ = return Nothing
    g = simpleFilter f

simpleFilter :: (InstantMessage -> Bool) -> (Stanza -> Bool)
simpleFilter f (MessageS m) = case getIM m of
        Just im -> f im
        Nothing -> False
simpleFilter _ _ = False

answerFilter :: (MessageBody -> Bool) -> (InstantMessage -> Bool)
answerFilter f (InstantMessage _ _ []) = False
answerFilter f (InstantMessage _ _ [x]) = f x
answerFilter f (InstantMessage _ _ xs) = any f xs


commandFilter' :: Text -> MessageBody -> Bool
commandFilter' _ (MessageBody _ "") = False
commandFilter' c (MessageBody _ xs) = c `T.isPrefixOf` xs

commandFilter = answerFilter . commandFilter'

extractJid :: Stanza -> Maybe Jid
extractJid (IQRequestS i) = iqRequestFrom i
extractJid (IQResultS i) = iqResultFrom i
extractJid (IQErrorS i) = iqErrorFrom i
extractJid (MessageS m) = messageFrom m
extractJid (MessageErrorS m) = messageErrorFrom m
extractJid (PresenceS p) = presenceFrom p
extractJid (PresenceErrorS p) = presenceErrorFrom p
