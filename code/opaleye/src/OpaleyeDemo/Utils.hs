{-# LANGUAGE FlexibleContexts #-}

module OpaleyeDemo.Utils
    ( -- * Exports
      printSql
    ) where

import Data.Profunctor.Product.Default
       ( Default )
import Opaleye
       ( Query, Unpackspec, showSqlForPostgres )

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres
