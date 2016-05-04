{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpaleyeDemo.Utils
    ( -- * Exports
      printSql
    , runQueryDebug
    ) where

import           Data.Profunctor.Product.Default (Default)
import           Database.PostgreSQL.Simple      (Connection)
import           Opaleye                         (Query, QueryRunner,
                                                  Unpackspec, runQuery,
                                                  showSqlForPostgres)

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

runQueryDebug :: (Default Unpackspec columns columns,
                  Default QueryRunner columns haskells) =>
                  Connection -> Query columns -> IO [haskells]
runQueryDebug c q = printSql q >> runQuery c q
