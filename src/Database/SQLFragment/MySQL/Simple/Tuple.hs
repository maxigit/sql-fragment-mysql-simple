{-# LANGUAGE TypeFamilies, DataKinds, OverloadedStrings, FlexibleContexts #-}
-- | Allow to select a 'SQLFragment' as tuple with a type signature
-- corresponding to SQLFragment type.
--
-- @
-- let itemCode = "code.items" ::  SQLFragment '[String] '[]
-- let itemPrice = "price.items" :: SQLFragment '[Double] '[]
--
-- main = do
--  connection <- ...
--  rows <- selectTuples connection (itemCode !&! itemPrice) 
--  -- rows :: [(String, Double)]
--  mapM_ print rows
-- @

module Database.SQLFragment.MySQL.Simple.Tuple where

-- standard
import Data.Int(Int64)
import Data.Monoid
import Data.String(IsString(..))
import System.Environment (lookupEnv)
import Control.Monad(when)
-- third-party
import qualified Database.MySQL.Simple as SQL
import qualified Database.MySQL.Simple.QueryResults as SQL
import qualified Database.MySQL.Simple.QueryParams as SQL
import Database.SQLFragment
import Database.SQLFragment.MySQL.Simple.Internal
import Database.SQLFragment.TypeFamilies
-- local

type family Tuple (e :: [*]) ::  *

-- | Execute an SQLFragment as a SELECT and return the result
-- as a list of tuples.
selectTuples :: (SQL.QueryResults (Tuple e), Show (Tuple e))
              =>
             SQL.Connection
             -> SQLFragment e '[]
             -> IO [Tuple e]
selectTuples conn query = select conn query

selectTuplesWith :: (SQL.QueryResults (Tuple e), Show (Tuple e), SQL.QueryParams (Tuple p), Show (Tuple p))
              =>
             SQL.Connection
             -> SQLFragment e p
             -> Tuple p
             -> IO [Tuple e]
selectTuplesWith conn query params = selectWith conn query params

insertTuples :: (SQL.QueryParams (Tuple e), Show (Tuple e))
             => SQL.Connection
             -> SQLFragment e '[]
             -> [Tuple e]
             -> IO Int64

insertTuples conn query values = insertMulti conn query values
-- * Instance

type instance Tuple '[a] = (SQL.Only (GetValue a))
type instance Tuple '[a, b] = ((GetValue a), (GetValue b))
type instance Tuple '[a, b, c] = ((GetValue a), (GetValue b), (GetValue c))
type instance Tuple '[a, b, c, d] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d))
type instance Tuple '[a, b, c, d, e] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e))
type instance Tuple '[a, b, c, d, e, f] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f))
type instance Tuple '[a, b, c, d, e, f, g] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g))
type instance Tuple '[a, b, c, d, e, f, g, h] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g), (GetValue h))
type instance Tuple '[a, b, c, d, e, f, g, h, i] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g), (GetValue h), (GetValue i))
type instance Tuple '[a, b, c, d, e, f, g, h, i, j] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g), (GetValue h), (GetValue i), (GetValue j))
type instance Tuple '[a, b, c, d, e, f, g, h, i, j, k] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g), (GetValue h), (GetValue i), (GetValue j), (GetValue k))
type instance Tuple '[a, b, c, d, e, f, g, h, i, j, k, l] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g), (GetValue h), (GetValue i), (GetValue j), (GetValue k), (GetValue l))
type instance Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g), (GetValue h), (GetValue i), (GetValue j), (GetValue k), (GetValue l), (GetValue m))
type instance Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m, n] = ((GetValue a), (GetValue b), (GetValue c), (GetValue d), (GetValue e), (GetValue f), (GetValue g), (GetValue h), (GetValue i), (GetValue j), (GetValue k), (GetValue l), (GetValue m), (GetValue n))

instance (Monoid a) =>  Monoid (SQL.Only a) where
    mempty = SQL.Only mempty
    mappend  a b = SQL.Only (SQL.fromOnly a `mappend` SQL.fromOnly b)
