{-# LANGUAGE OverloadedStrings #-}

module Simple.Commands
    ( -- * Exports
      runAndPrintCommand
      -- * Commands
    , Command(..)
     -- * Flags
    , Flag(..)
    ) where

import Data.Maybe                                (fromJust)
import Data.List                                 (intercalate)
import qualified Data.ByteString.Char8 as B      (pack)
import qualified Simple.Todo as T
import qualified Simple.Hashtag as H
import           Database.PostgreSQL.Simple      (Connection)
import           Database.PostgreSQL.Simple.Time (Date, parseDate)

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
    | Help                  --help
    | Version               --version
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Main command runner
runAndPrintCommand :: Connection -> Command -> [Flag] -> IO ()
runAndPrintCommand conn (Find x) flags     = runFindCommand conn x flags
runAndPrintCommand conn (Add  x) flags     = runAddCommand conn x flags
runAndPrintCommand conn (Complete x) flags = runCompleteCommand conn x flags
runAndPrintCommand conn List flags         = runListCommand conn flags

runFindCommand :: Connection -> Int -> [Flag] -> IO ()
runFindCommand c tid _ = do
    todo     <- T.findTodo c tid
    hashtags <- H.allHashtagsForTodo c tid
    putStrLn $ unlines [ "Todo: "     ++ show tid
                       , "Title: "    ++ T.getTitle todo
                       , "Due by: "   ++ show (T.getDueDate todo)
                       , "Priority: " ++ priority (T.getPrio todo)
                       , "Hashtags: " ++ showHashtags hashtags
                       ]
    where
        priority Nothing  = "-"
        priority (Just d) = show d

runAddCommand :: Connection -> String -> [Flag] -> IO ()
runAddCommand _ desc flags = do
    let priority = prioFromFlags flags
    let todo = if priority /= Nothing then
                  T.Todo { T.getId      = Nothing
                         , T.getTitle   = desc
                         , T.getDueDate = dueDateFromFlags flags
                         , T.getPrio    = priority
                         }
               else
                  T.Todo { T.getId      = Nothing
                         , T.getTitle   = desc
                         , T.getDueDate = dueDateFromFlags flags
                         , T.getPrio    = Nothing
                         }
    putStrLn (show todo)


runCompleteCommand :: Connection -> Int -> [Flag] -> IO ()
runCompleteCommand = undefined

runListCommand :: Connection -> [Flag] -> IO ()
runListCommand c flags = do
    todos <- if OrderByPriority `elem` flags then
                T.allTodosByPrio c
             else
                T.allTodos c
    mapM_ (printTodo flags c) todos

printTodo :: [Flag] -> Connection -> T.Todo -> IO ()
printTodo flags c todo = do
    hashtags <- if WithHashtags `elem` flags then
                   H.allHashtagsForTodo c (fromJust $ T.getId todo)
                else
                   return [] :: IO [H.Hashtag]

    putStrLn $ intercalate " " $
        [ (show . fromJust $ T.getId todo) ++ ". "
        , T.getTitle todo
        , "(due by: " ++ (show $ T.getDueDate todo) ++ ")"
        , "(priority: " ++ (maybe "-" show (T.getPrio todo)) ++ ")"
        , showHashtags hashtags
        ]

--------------------------------------------------------------------------------
-- | Helper function to show comma-separated hashtags
showHashtags :: [H.Hashtag] -> String
showHashtags hs = intercalate ", " $ map H.getHashtag hs

--------------------------------------------------------------------------------
-- | Helper function to get the due date from the list of flags
dueDateFromFlags :: [Flag] -> Date
dueDateFromFlags []            = error "Must specify due date!"
dueDateFromFlags ((DueBy s):_) = either
                                 (\e  -> error e)
                                 id
                                 (parseDate (B.pack ("\"" ++ s ++ "\"")))
dueDateFromFlags (_:xs)        = dueDateFromFlags xs

-- | Helper function to get the priority setting from the list of flags
prioFromFlags :: [Flag] -> Maybe Int
prioFromFlags []                  = Nothing
prioFromFlags ((SetPriority p):_) = Just (read p :: Int)
prioFromFlags (_:xs)              = prioFromFlags xs
