{-# LANGUAGE OverloadedStrings #-}
-- | Generates from a database a schema file suitable for
-- to generate 'SQLFragment' using the SQLFragment generator.
-- The file is space separated value with the following column
--   - name : identifier for the sql-fragment. Should be a valid haskell name
--   - table : SQL table name
--   - column : SQL column name
--   - haskell type : haskell type in which value will be converted to
--   - label : use to identify HList.Record fields. any string will do.
module Main where

-- standard
import Data.Char (toUpper, toLower, isLetter)
import Data.List.Split (splitOn)
import qualified Data.List as List
import System.Console.GetOpt as O
import System.Environment
-- third party
import qualified Database.MySQL.Simple as SQL

-- local module

-- | processes column information and displays it.
-- The output corresponds to the information needed to generate
-- the fragments, i.en
-- fragmentName column table type
processColumns :: Options ->  ( String , String, String, String) -- ^ column table type
                  -> String -- ^ string to 
processColumns opts (column, table, sqlType, nullable) =
    List.intercalate " " [name, table, column, hType, (show $ nullB nullable), label] where
        name = naming opts table column
        label = labelling opts table column
        hType = htype sqlType
        nullB "NO" = False
        nullB "YES" = True
-- * Type mapping
htype :: String -> String
htype "varchar" = "String"
htype "bigint" = "Integer"
htype "longtext" = "String"
htype "datetime" = "Time.UTCTime"
htype "int" = "Int"
htype "decimal" = "Ratio"
htype "text" = "String"
htype "longblob" = "String"
htype "tinyint" = "Bool"
htype "smallint" = "Int"
htype "mediumtext" = "String"
htype "blob" = "String"
htype "tinytext" = "String"
htype "mediumint" = "String"
htype "float" = "Float"
htype "double" = "Double"
htype "date" = "Time.Day"
htype "timestamp" = "Time.UTCTime"
htype "char" = "String"
htype "tinyblob" = "String"
htype "varbinary" = "String"
-- htype "set" = "String"
-- htype "enum" = "String"
htype "time" = "Time.TimeOfDay"
htype s = "<-- Please edit me " ++  s ++ " -->"

-- | Options
data Options = Options
    { connectInfo :: SQL.ConnectInfo -- ^ database info
    , labelling :: String -> String -> String -- ^ function to generate label name
    , naming :: String -> String -> String -- ^ function to generate fragment name
    }

defaultOptions = Options SQL.defaultConnectInfo labelByColumn snakeCase
    
options  :: [O.OptDescr (Options -> Options)]
options = 
    [ O.Option "h" ["host"] 
        (setIf (\o a -> o {connectInfo = (connectInfo o) { SQL.connectHost = a }}) "HOST") "host"
    , O.Option "u" ["user"] 
        (setIf (\o a -> o {connectInfo = (connectInfo o) { SQL.connectUser = a }}) "USER") "user"
    , O.Option "p" ["password"]  
        (setIf (\o a -> o {connectInfo = (connectInfo o)  { SQL.connectPassword = a }}) "PASSWORD") "password"
    , O.Option "P" ["port"] 
        (setIf (\o a -> o {connectInfo = (connectInfo o)  { SQL.connectPort = (read a) }}) "PORT") "port"
    , O.Option "q" ["camelQuote"] 
        (O.NoArg (\o -> o {labelling = labelByCamelColumn, naming = camelQuote})) "camel quote"
    ] 
    where setIf f s = O.OptArg (\arg opt ->  maybe opt (f opt) arg) s


-- * Main
main :: IO ()
main =  do
    args   <- getArgs
    case O.getOpt O.Permute options args of
        (actions, [database], []) -> 
            let opt = foldl (flip id) defaultOptions actions
            in do
                conn <- SQL.connect (connectInfo opt)
                rows <- SQL.query  conn "\
                    \ SELECT column_name \
                    \      , table_name \
                    \      , data_type \
                    \      , is_nullable \
                    \ FROM information_schema.columns \
                    \ WHERE table_schema = ? \
                    \ AND table_name not like '% %' \
                    \ AND column_name not like '% %' \
                    \ ORDER BY column_name \
                    \" [database :: String]
                mapM_ (putStrLn.processColumns opt) rows
        (_, _, errors) -> error $ concat errors ++ O.usageInfo header options 
    where header = "Usage: DumpSchema [OPTIONS] DATABASE"
        
    
    
-- * Naming strategies
-- ** fragment name
snakeCase :: String -- ^ table
          -> String -- ^ column
          -> String

snakeCase table column = uncapitalize table ++ "__" ++ uncapitalize column

camelQuote :: String -- ^ table
          -> String -- ^ column
          -> String
camelQuote table column = camelCase $ dropNonLetterPrefix table ++ "'" ++ dropNonLetterPrefix column 

-- ** Labelling
labelByColumn :: String -- ^ table
          -> String -- ^ column
          -> String
labelByColumn table column = uncapitalize column
    
labelByCamelColumn :: String -- ^ table
          -> String -- ^ column
          -> String
labelByCamelColumn table column = camelCase column
-- * Cases management

dropNonLetterPrefix :: String -> String
dropNonLetterPrefix [] = []
dropNonLetterPrefix (x:xs)
    | isLetter x = (x:xs)
    | otherwise = dropNonLetterPrefix xs

camelCase :: String -> String
camelCase [] = []
camelCase (xs) =  let
    words = splitOn "_"  xs
    uppers = concatMap capitalize words
    in uncapitalize uppers
    

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (x:xs) = toLower x : xs



