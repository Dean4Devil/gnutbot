module Gnut.Permissions
    ( Permissions
    , Permission(..)
    , PermPath(..)
    , PermDesc(..)

    , checkAll
    , checkAllOr
    , check
    , checkOr
    , filterPerm
    , matches
    )
    where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Network.Xmpp.Internal

import Data.List
import Data.Map.Lazy hiding (filter)
import Data.Maybe

import Data.Yaml (FromJSON(..))
import qualified Data.Yaml as Y

import Data.Text (Text)
import qualified Data.Text as T

type Permissions = [Permission]

-- Hidden behind newtype for custom Show & Ord implementations
newtype PermPath = PermPath [Text]
    deriving (Eq)
instance Show PermPath where
    show (PermPath p) = T.unpack $ T.concat (intersperse "." p)
instance Ord PermPath where
    compare (PermPath x) (PermPath y) = compare (length x) (length y)


data PermDesc = Precise PermPath  -- a.b.c
              | Wildcard PermPath -- a.b.* (path = [a,b])
    deriving (Eq)
instance Show PermDesc where
    show (Precise path)  = show path
    show (Wildcard path) = show path ++ ".*"
instance Ord PermDesc where
    compare (Precise x) (Precise y) = compare x y
    compare (Wildcard x) (Wildcard y) = compare x y

    compare (Precise x) (Wildcard y) = case compare x y of
            EQ -> LT -- x and y are of the same length, but y is a Wildcard for everything *after* that path
            x -> x

    compare x@(Wildcard _) y@(Precise _) = revOrd $ compare y x
      where revOrd LT = GT
            revOrd EQ = EQ
            revOrd GT = LT

data Permission = Positive PermDesc
                | Negative PermDesc
  deriving (Eq)
instance Show Permission where
    show (Positive path) = show path
    show (Negative path) = "~" ++ show path
instance Ord Permission where
    compare (Positive x) (Positive y) = compare x y
    compare (Positive x) (Negative y) = compare x y
    compare (Negative x) (Positive y) = compare x y
    compare (Negative x) (Negative y) = compare x y
instance FromJSON Permission where
    parseJSON (Y.String v) = case T.head v of
        '~' -> do
            let path = T.splitOn "." (T.tail v)
            return $ Negative (desc path)
        _   -> do
            let path = T.splitOn "." v
            return $ Positive (desc path)
      where desc xs = case last xs of
                "*" -> Wildcard (PermPath (init xs))
                _   -> Precise (PermPath xs)
    parseJSON _ = error "Permission must be a string"

checkAllOr :: Bool -> PermPath -> [[Permission]] -> Bool
checkAllOr def p xs = fromMaybe def (checkAll p xs)

checkOr :: Bool -> PermPath -> [Permission] -> Bool
checkOr def p ps = fromMaybe def (check p ps)

-- Returns `Just` the value of a permission if one is found in the set and
-- Nothing if no matching permission is found
checkAll :: PermPath -> [[Permission]] -> Maybe Bool
checkAll p [] = Nothing
checkAll p (x:xs) = case check p x of
    Nothing -> checkAll p xs
    j -> j

-- Returns `Just` the value of a permission if one is found and Nothing if
-- no matching permission is found
check :: PermPath -> [Permission] -> Maybe Bool
check p ps = case filterPerm p ps of
    [] -> Nothing
    xs -> case maximum xs of
        (Positive _) -> Just True
        (Negative _) -> Just False

filterPerm :: PermPath -> [Permission] -> [Permission]
filterPerm p = filter (matches p)

-- Check if a given permission matches but ignore it's value
matches :: PermPath -> Permission -> Bool
matches p (Positive d) = matches' p d
matches p (Negative d) = matches' p d

matches' :: PermPath -> PermDesc -> Bool
matches' p (Precise c)  = p == c
-- Length check is needed because a.b.c is a subset of a.b.c
matches' (PermPath x) (Wildcard (PermPath y)) = (y `isPrefixOf` x) && (length y < length x)

-- Ordering:
-- compare a.b.c d.e.f == EQ -- Same precision
-- compare a.b.c a.b.* == GT -- a.b.c is preciser than a.b.*
-- compare a.b.c a.b.c.* == LT -- a.b.c is less precise than a.b.c.*
-- compare a.b.* d.e.* == EQ
--
-- Local < Global
-- I mean if you are a global admin, what's the point in forbidding you
-- rights in a specific channel?
--
-- Role < Personal
-- So, if you get a command in a MUC Gnut should checks:
-- Global.Personal -> Global.Role -> <Channel>.Personal -> <Channel>.Role -> Default
-- for the first check that does not return 'Undefined'
