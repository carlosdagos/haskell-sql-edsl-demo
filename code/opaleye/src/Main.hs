module Main where

import Opaleye
import Database.PostgreSQL.Simple
import qualified OpaleyeDemo.Utils as U
import qualified OpaleyeDemo.Todo  as T

main :: IO ()
main = do
    conn  <- connect defaultConnectInfo
    todos <- U.runQueryDebug conn T.todoQuery :: IO [T.Todo]
    mapM_ print todos
