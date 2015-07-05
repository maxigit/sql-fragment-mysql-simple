{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators , OverloadedStrings
 , PolyKinds
 , ScopedTypeVariables
 , FlexibleInstances
 , FlexibleContexts #-}
-- | Allow to select a 'SQLFragment' as a 'Record'. The labels and the
-- type of each fields are extracted from the SQLFragment type.
--
-- @
-- let itemCode = "code.items" ::  SQLFragment '[Tagged Code String] '[]
-- let itemPrice = "price.items" :: SQLFragment '[Tagged Price Double] '[]
-- type Code = Label "code"
-- type Price = Label "price"
--
-- main = do
--  connection <- ...
--  rows <- selectRecords connection (itemCode !&! itemPrice) 
--  -- rows :: Record of Code [(String, Double)]
--  mapM_ print rows
-- @
module Database.SQLFragment.MySQL.Simple.Record where

-- standard
import Data.Int(Int64)
-- third party
import qualified Database.MySQL.Simple as SQL
import qualified Database.MySQL.Simple.QueryResults as SQL
import qualified Database.MySQL.Simple.QueryParams as SQL
import qualified Database.MySQL.Simple.Result as SQL
import Database.MySQL.Simple
import Data.HList(Label(..))
import Data.HList.Record(Record, HRLabelSet, emptyRecord, (.=.), (.*.), recordValues, RecordValues)
import Data.Tuple.HList -- (fromHList, toHList)
import Data.Tagged(Tagged)
-- local
import Database.SQLFragment
import Database.SQLFragment.MySQL.Simple.Internal
import Database.SQLFragment.MySQL.Simple.Tuple


-- | Execute an SQLFragment as a SELECT and return the result
-- as a list of records.
selectRecords :: ( SQL.QueryResults (Record e)
                 , Show (Record e))
              => SQL.Connection
              -> SQLFragment e '[]
              -> IO [Record e]
selectRecords conn query = select conn query

-- | Execute an SQLFragment with paramters as a SELECT and return the result
-- as a list of records.
selectRecordsWith :: ( SQL.QueryResults (Record e) , Show (Record e)
                 , SQL.QueryParams (Tuple p), Show (Tuple p)
                 )
              => SQL.Connection
              -> SQLFragment e p
              -> (Tuple p)
              -> IO [Record e]
selectRecordsWith conn query params = selectWith conn query params


{-
insertRecords :: ( SQL.QueryParams (Tuple e)
                 , RecordValues e
                 , HLst (Tuple e) (RecordValue
                 , Show (Record e), Show (Tuple e)
                 )
              => SQL.Connection
              -> SQLFragment e '[]
              -> [Record e]
              -> IO Int64
-}
insertRecords conn query records = insertTuples conn query (map recordToTuple records)

instance SQL.QueryResults (Record '[]) where
    convertResults _ _ = emptyRecord

instance (SQL.Result a, SQL.QueryResults (Record e), HRLabelSet (Tagged l a ': e)) => SQL.QueryResults (Record (Tagged (l :: k)  a ': e)) where
    convertResults (f:fs) (v:vs) = (Label :: Label l) .=. r .*. record where
        r = SQL.convert f v :: a
        record = SQL.convertResults fs vs :: Record e
        
-- * Records Tuples Conversion
-- recordToTuple :: RecordValues e => Record e -> (Tuple e)
recordToTuple r = fromHList $ recordValues r

