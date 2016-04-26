{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module HRR.Reports
    ( -- * Exports
      runReports
    , todosWithoutHashtags
    , todosMultipleHashtags
    , hashtagsWithMultipleTodos
    , countLateTodos
    , countFutureTodos
   ) where

import           Database.Relational.Query
import           Database.HDBC
                 ( IConnection )
import           Data.Int
                 ( Int32 )
import           Data.Time.Clock
                 ( getCurrentTime, utctDay )
import           Data.Time.Calendar
                 ( Day )
import qualified HRR.Todo                   as T
import qualified HRR.Hashtag                as H
import qualified HRR.ConnectionHelpers      as C

runReports :: (IConnection conn) => conn -> IO ()
runReports conn = do
    putStrLn "Todos without hashtags:"
    C.run conn () todosWithoutHashtags

    now <- fmap utctDay getCurrentTime

    putStrLn "Number of late todos:"
    C.run conn now countLateTodos

    putStrLn "Number of upcoming todos:"
    C.run conn now countFutureTodos

    putStrLn "Todos with multiple hashtags:"
    C.run conn () todosMultipleHashtags

    putStrLn "Hashtags with multiple todos:"
    C.run conn () hashtagsWithMultipleTodos

todosWithoutHashtags :: Relation () (Int32, String)
todosWithoutHashtags = relation $ do
    t <- query T.todo
    h <- queryMaybe H.hashtag
    on $ just (t ! T.id') .=. h ?! H.todoId'
    wheres $ isNothing (h ?! H.todoId')
    return $ t ! T.id' >< t ! T.title'

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

countLateTodos :: Relation Day Int
countLateTodos = aggregateRelation' . placeholder $ \ph -> do
    t <- query T.todo
    wheres $ t ! T.dueDate' .<=. ph
    return $ count (t ! T.id')

countFutureTodos :: Relation Day Int
countFutureTodos = aggregateRelation' . placeholder $ \ph -> do
    t <- query T.todo
    wheres $ t ! T.dueDate' .>. ph
    return $ count (t ! T.id')


