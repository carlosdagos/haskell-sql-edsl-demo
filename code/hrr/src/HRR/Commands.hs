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
import           Database.HDBC
                 ( IConnection, SqlValue, commit )
import           Database.HDBC.Record
                 ( runQuery, runInsert, runInsertQuery, runInsert)
import           Database.Record
                 ( ToSql, FromSql )
import           Data.Time.Calendar
                 ( Day, fromGregorian )
import           Data.Int
                 ( Int32 )
import qualified Data.ByteString.Char8 as B
                 ( unpack, pack, split )
import qualified HRR.Todo                   as T
import qualified HRR.Hashtag                as H

--------------------------------------------------------------------------------
-- | Commands available for the application
data Command
    = List                  -- list
    | Find Int32            -- find
    | Add String            -- add
    | Complete Int32        -- complete
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
      Find x     -> runFindCommand conn x flags
      Add title  -> runAlternativeAddCommand conn title flags
      Complete x -> runCompleteCommand conn x

runListCommand :: (IConnection conn) => conn -> [Flag] -> IO ()
runListCommand conn flags = do
    let q = if OrderByPriority `notElem` flags then
                T.todo
            else
                -- FIXME: Manage to get NULLS LAST in order-by
                relation $ do
                    t <- query T.todo
                    desc $ t ! T.prio'
                    return t

    if Debug `elem` flags then
        runDebug conn () q
    else
        run conn () q

runAddCommand :: (IConnection conn) => conn -> String -> [Flag] -> IO ()
runAddCommand conn title flags = do
    let dueDate = dueDateFromFlags flags
    let insertR = relation . return $ T.PiTodo |$| value title
                                               |*| value dueDate
                                               |*| value (Just 11)

    let insertQ = derivedInsertQuery T.piTodo' insertR
    i <- runInsertQuery conn insertQ ()
    -- FIXME: What about RETURNING id?
    commit conn
    print i

--------------------------------------------------------------------------------
-- | Alternative insert command avoiding the use of hrr "applicatives", would
-- | remove the need to declare the Pi datas for partial record information
runAlternativeAddCommand :: (IConnection conn)
                         => conn -> String -> [Flag] -> IO ()
runAlternativeAddCommand conn title flags = do
    let dueDate    = dueDateFromFlags flags
    let insertTodo = derivedInsertValue $ do
                        T.title'   <-# value title
                        T.dueDate' <-# value dueDate
                        T.prio'    <-# value (Just 11)
                        return unitPlaceHolder
    i <- runInsert conn insertTodo ()
    commit conn
    print i


runFindCommand :: (IConnection conn) => conn -> Int32 -> [Flag] -> IO ()
runFindCommand conn x flags =
    if Debug `elem` flags then
        runQDebug conn x T.selectTodo
    else
        runQ conn x T.selectTodo

runCompleteCommand :: (IConnection conn) => conn -> Int32 -> IO ()
runCompleteCommand conn x = do
    let deleteQ = typedDelete T.tableOfTodo . restriction $
                    \projection ->
                        wheres $ projection ! T.todoId' .=. value x

    print deleteQ

--------------------------------------------------------------------------------
-- | Helper function to get the due date from the list of flags
dueDateFromFlags :: [Flag] -> Day
dueDateFromFlags []           = error "Must specify due date!"
dueDateFromFlags (DueBy s:_) = fromGregorian year month day
                                 where
                                   ymd   = B.split '-' (B.pack s)
                                   year  = read (B.unpack (ymd !! 0)) :: Integer
                                   month = read (B.unpack (ymd !! 1)) :: Int
                                   day   = read (B.unpack (ymd !! 2)) :: Int
dueDateFromFlags (_:xs)       = dueDateFromFlags xs

--------------------------------------------------------------------------------
-- | Run a relation to the connection but first log the relation
runDebug :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
         => conn -> p -> Relation p a -> IO ()
runDebug conn param rel = runQDebug conn param (relationalQuery rel)

--------------------------------------------------------------------------------
-- | Run a relation to the connection
run :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
    => conn -> p -> Relation p a -> IO ()
run conn param rel = runQ conn param (relationalQuery rel)

--------------------------------------------------------------------------------
-- | Run a relation to the connection but first log the relation
runQDebug :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
          => conn -> p -> Query p a -> IO ()
runQDebug conn param rel = do
  putStrLn $ "sql debug: " ++ show rel
  records <- runQuery conn rel param
  mapM_ print records

--------------------------------------------------------------------------------
-- | Run a relation to the connection
runQ :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
    => conn -> p -> Query p a -> IO ()
runQ conn param rel = do
  records <- runQuery conn rel param
  mapM_ print records
