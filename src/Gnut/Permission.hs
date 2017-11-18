module Gnut.Permission
    ( Permission(..)
    , Permissions
    )
    where

import Gnut.Types

import Data.List
import Data.Map.Lazy

import Data.Text (Text)
import qualified Data.Text as T

data Permission
    = Positive [Text]
    | Negative [Text]
  deriving (Eq)

instance Show Permission where
    show (Positive path) = T.unpack $ T.concat (intersperse "." path)
    show (Negative path) = T.unpack $ T.concat $ "-" : intersperse "." path

type Permissions = [Permission]

check' :: Permssions -> Permission -> Bool
check' Just _ = True
check' Nothing = False

check :: Permissions -> Permission -> Maybe Permission
check ps p = do
    foldl (\perm,fold_perm -> )
  where
    precision = length
