{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module OpaleyeDemo.Reports where

import           Control.Arrow              (returnA)
import           Data.Int                   (Int64)
import           Data.Time.Calendar         (Day)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                    hiding (not, null)
import           OpaleyeDemo.Flags
import qualified OpaleyeDemo.Hashtag        as H
import qualified OpaleyeDemo.Ids            as I
import qualified OpaleyeDemo.Todo           as T
import qualified OpaleyeDemo.Utils          as U

todosAndHashtags :: Query (T.TodoColumns, H.HashtagNullableColumns)
todosAndHashtags = leftJoin T.todoQuery H.hashtagQuery eqTodoId
    where eqTodoId (todos, hashtags) = T._id todos .=== H._todoId hashtags

todosWithoutHashtags :: Query T.TodoColumns
todosWithoutHashtags = proc () -> do
    (todos, hashtags) <- todosAndHashtags -< ()
    restrict -< isNull ((I.todoId . H._todoId) hashtags)
    returnA -< todos

todosOpDate
    :: (Column PGDate -> Column PGDate -> Column PGBool)
    -> QueryArr Day T.TodoColumns
todosOpDate op = proc day -> do
    todos <- T.todoQuery -< ()
    restrict -< T._dueDate todos `op` pgDay day
    returnA -< todos

lateTodos :: Day -> Query T.TodoColumns
lateTodos day = proc () -> do
    todos <- todosOpDate (.<=) -< day
    returnA -< todos

futureTodos :: Day -> Query T.TodoColumns
futureTodos day = proc () -> do
    todos <- todosOpDate (.>) -< day
    returnA -< todos

countLateTodos :: Day -> Query (Column PGInt8)
countLateTodos day
  = aggregate count . fmap (I.todoId . T._id) $ lateTodos day

countFutureTodos :: Day -> Query (Column PGInt8)
countFutureTodos day
  = aggregate count . fmap (I.todoId . T._id) $ futureTodos day


{-
-- What I want
--
-- select t.id, t.title
-- from todo t
-- join hashtag h
-- on t.id = h.todo_id
-- group by t.id
-- having count(h.hashtag_str) > 1
--
todosMultipleHashtags :: Relation () Int32
todosMultipleHashtags = undefined
--aggregateRelation $ do
--    t <- query T.todo
--    h <- query H.hashtag
--    on $ t ! T.id' .=. h ! H.todoId'
--    g <- groupBy $ t ! T.id'
--    having $ count (h ! H.hashtagStr') .>. value 1
--    return g

hashtagsWithMultipleTodos :: Relation () String
hashtagsWithMultipleTodos = undefined

--
-- What I want
--
-- select hashtag_str, count(*)
-- from hashtag
-- group by hashtag_str
-- having count(hashtag_str) > 1
-- order by count(hashtag_str) desc;
--
mostPopularHashtags :: Relation () String
mostPopularHashtags = undefined
--do
--    h <- query H.hashtag
--    g <- groupBy $ h ! H.hashtagStr'
--    having $ count (h ! H.hashtagStr') .>. value 1
--    desc $ count (h ! H.hashtagStr')
--    return g
-}

runReports :: Connection -> [Flag] -> IO ()
runReports conn flags = do
--    putStrLn "Most popular hashtags:"
--    runQuery mostPopularHashtags

    putStrLn "Todos without hashtags:"
    twh <- if Debug `elem` flags then
             U.runQueryDebug conn todosWithoutHashtags :: IO [T.Todo]
           else
             runQuery conn todosWithoutHashtags :: IO [T.Todo]

    mapM_ print twh
    now <- fmap utctDay getCurrentTime

    putStrLn "Number of late todos:"
    lt <- if Debug `elem` flags then
            U.runQueryDebug conn (countLateTodos now) :: IO [Int64]
          else
            runQuery conn (countLateTodos now) :: IO [Int64]

    if null lt then
      print (0 :: Int)
    else
      print . head $ lt

    putStrLn "Number of upcoming todos:"
    ft <- if Debug `elem` flags then
            U.runQueryDebug conn (countFutureTodos now) :: IO [Int64]
          else
            runQuery conn (countFutureTodos now) :: IO [Int64]

    if null ft then
      print (0 :: Int)
    else
      print . head $ ft

--    putStrLn "Todos with multiple hashtags:"
--    runQuery todosMultipleHashtags

--    putStrLn "Hashtags with multiple todos:"
--    runQuery hashtagsWithMultipleTodos


