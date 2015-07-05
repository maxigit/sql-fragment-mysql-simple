{-# LANGUAGE TypeFamilies, DataKinds, OverloadedStrings, FlexibleContexts #-}

module Database.SQLFragment.MySQL.Simple.Internal
( select
, selectWith
, insertMulti
) where

-- standard
import System.Environment (lookupEnv)
import Control.Monad(when)
import Data.String (fromString)
import Data.Int(Int64)
-- third-party
import qualified Database.MySQL.Simple as SQL
import qualified Database.MySQL.Simple.QueryResults as SQL
import qualified Database.MySQL.Simple.QueryParams as SQL
import Database.SQLFragment
import Database.SQLFragment.TypeFamilies
-- local

-- | Select an SQLFragment with no parameters.
select :: (SQL.QueryResults r, Show r)
       => SQL.Connection
       -> SQLFragment e '[]
       -> IO [r]
select conn query = do
    traceQuery <- lookupEnv("HH_TRACE_QUERY")
    when (isSet traceQuery)$ putStrLn sql

    result <- SQL.query_ conn (fromString sql)

    traceResult <- lookupEnv("HH_TRACE_RESULT")
    when (isSet traceResult)$ mapM_ print result

    return result
    where sql = toSelectQuery query

-- | Select an SQLFragment with parameters.
selectWith :: ( SQL.QueryResults r, Show r
              , SQL.QueryParams p, Show p)
       => SQL.Connection
       -> SQLFragment e' p'
       -> p
       -> IO [r]
selectWith conn query params = do
    traceQuery <- lookupEnv("HH_TRACE_QUERY")
    when (isSet traceQuery)$ putStrLn sql

    traceParam <- lookupEnv("HH_TRACE_PARAMS")
    when (isSet traceParam) $ putStrLn (show params)

    result <- SQL.query conn (fromString sql) params

    traceResult <- lookupEnv("HH_TRACE_RESULT")
    when (isSet traceResult)$ mapM_ print result

    return result
    where sql = toSelectQuery query


insertMulti :: (SQL.QueryParams e', Show e') 
           => SQL.Connection
           -> SQLFragment e '[]
           -> [e']
           -> IO Int64
insertMulti conn query values = do
    traceQuery <- lookupEnv("HH_TRACE_QUERY")
    when (isSet traceQuery)$ putStrLn sql >>  putStrLn (show values)

    traceParam <- lookupEnv("HH_TRACE_PARAMS")
    when (isSet traceParam) $ putStrLn (show values)

    result <- SQL.withTransaction conn (SQL.executeMany conn (fromString sql) values)

    traceResult <- lookupEnv("HH_TRACE_RESULT")
    when (isSet traceResult)$ print result
    return result
    where sql = toInsertQuery query
    


-- | Check if environment variable result is considered ON or OFF.
isSet :: Maybe String -> Bool
isSet m = case m of 
            Just "1" -> True
            Just "y" -> True
            Just "Y" -> True
            Just "yes" -> True
            Just "YES" -> True
            Just "Yes" -> True
            _ -> False
