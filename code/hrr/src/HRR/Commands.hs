{-# LANGUAGE FlexibleContexts #-}

module HRR.Commands
    ( -- * Exports
      runAndPrintCommand
    , run
    , runDebug
      -- * Commands
    , Command(..)
     -- * Flags
    , Flag(..)
    ) where

import           Database.Relational.Query
import           Database.HDBC                    (IConnection, SqlValue)
import           Database.HDBC.Record.Query       (runQuery)
import           Database.Record                  (ToSql, FromSql)
import qualified HRR.Todo                   as T
import qualified HRR.Hashtag                as H

--------------------------------------------------------------------------------
-- | Commands available for the application
data Command
    = List                  -- list
    | Find Int              -- find
    | Add String            -- add
    | Complete Int          -- complete
    deriving (Show)

--------------------------------------------------------------------------------
-- | Flags available for the app
data Flag
    = DueBy String          --due-by
    | Late                  --late
    | WithHashtags          --with-hashtags
    | SearchHashtag String  --hashtags
    | OrderByPriority       --order-by-priority
    | SetPriority String    --priority
    | Debug                 --debug
    | Help                  --help
    | Version               --version
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Main command runner
runAndPrintCommand :: (IConnection conn) => conn -> Command -> [Flag] -> IO ()
runAndPrintCommand conn cmd flags
  = case cmd of
      List       -> runListCommand conn flags
      Find _     -> putStrLn "find!"
      Add _      -> putStrLn "add!"
      Complete _ -> putStrLn "complete!"


runListCommand :: (IConnection conn) => conn -> [Flag] -> IO ()
runListCommand conn flags = do
    if Debug `elem` flags then
        runDebug conn () T.todo
    else
        run conn () T.todo

--------------------------------------------------------------------------------
-- | Run a relation to the connection but first log the relation
runDebug :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
         => conn -> p -> Relation p a -> IO ()
runDebug conn param rel = do
  putStrLn $ "sql debug: " ++ show rel
  records <- runQuery conn (relationalQuery rel) param
  mapM_ print records

--------------------------------------------------------------------------------
-- | Run a relation to the connection
run :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
    => conn -> p -> Relation p a -> IO ()
run conn param rel = do
  records <- runQuery conn (relationalQuery rel) param
  mapM_ print records

