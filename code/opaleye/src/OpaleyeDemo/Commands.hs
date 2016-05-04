{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module OpaleyeDemo.Commands
    ( -- * Exports
      runAndPrintCommand
      -- * Commands
    , Command(..)
     -- * Flags
    , Flag(..)
    ) where

import           Data.List                       (intercalate)
import           Data.Maybe                      (fromJust, isJust, isNothing)
import           Data.Time.Calendar              (fromGregorian)
import           Database.PostgreSQL.Simple      (Connection, Only (..))
import           Database.PostgreSQL.Simple.Time (Date, parseDate)
import           Opaleye
import qualified OpaleyeDemo.Hashtag             as H
import qualified OpaleyeDemo.Ids                 as I
import qualified OpaleyeDemo.Todo                as T
import qualified OpaleyeDemo.Utils               as U
import           System.Exit
import           System.IO

--------------------------------------------------------------------------------
-- | Commands available for the application
data Command
    = List                  -- ^ "list" command
    | Find Int              -- ^ "find" command
    | Add String            -- ^ "add" command
    | Complete Int          -- ^ "complete" command
    deriving (Show)

--------------------------------------------------------------------------------
-- | Flags available for the app
data Flag
    = DueBy String          -- ^ --due-by
    | Late                  -- ^ --late
    | WithHashtags          -- ^ --with-hashtags
    | SearchHashtag String  -- ^ --hashtags
    | OrderByPriority       -- ^ --order-by-priority
    | SetPriority String    -- ^ --priority
    | Help                  -- ^ --help
    | Version               -- ^ --version
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Main command runner
runAndPrintCommand :: Connection -> Command -> [Flag] -> IO ()
runAndPrintCommand conn (Find x) flags = runFindCommand conn x flags
runAndPrintCommand conn (Add x) flags  = runAddCommand conn x flags
runAndPrintCommand conn (Complete x) _ = runCompleteCommand conn x
runAndPrintCommand conn List flags     = runListCommand conn flags

runFindCommand :: Connection -> Int -> [Flag] -> IO ()
runFindCommand conn x flags = do
    let tid = I.TodoId { I.todoId = x }
    todos <- runQuery conn (T.selectTodo tid) :: IO [T.Todo]

    if length todos == 1 then
      printTodo conn flags (head todos)
    else
      hPutStrLn stderr "Todo not found!" >> exitWith (ExitFailure 1)

runAddCommand :: Connection -> String -> [Flag] -> IO ()
runAddCommand _ _ _ = putStrLn "addCommand"

runCompleteCommand :: Connection -> Int -> IO ()
runCompleteCommand _ _ = putStrLn "completeCommand"

runListCommand :: Connection -> [Flag] -> IO ()
runListCommand conn flags = do
    todos <- runQuery conn T.todoQuery :: IO [T.Todo]
    mapM_ (printTodo conn flags) todos

printTodo :: Connection -> [Flag] -> T.Todo -> IO ()
printTodo conn flags todo = do
    hashtags <- if WithHashtags `elem` flags then
                  runQuery conn (H.hashtagsForTodo (T._id todo)) :: IO [H.Hashtag]
                else
                  return []

    mapM_ print hashtags
    print todo
