{-# LANGUAGE FlexibleContexts #-}

module OpaleyeDemo.Utils
    ( -- * Exports
      printSql
    , runQueryDebug
    ) where

import Database.PostgreSQL.Simple
       ( Connection )
import Data.Profunctor.Product.Default
       ( Default )
import Opaleye
       ( Query, QueryRunner, Unpackspec, showSqlForPostgres, runQuery )

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

runQueryDebug :: (Default Unpackspec columns columns,
                  Default QueryRunner columns haskells) =>
                  Connection -> Query columns -> IO [haskells]
runQueryDebug c q = do
    printSql q
    runQuery c q
