{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simple.Hashtag
    ( -- * Exports
      allHashtags
    , allHashtagsForTodo
    , allHashtagsWithTodos
    , getTodoId
    , getHashtag
    , getHashtagTodoId
    , getHashtagHash
    , getTitle
    , getDueDate
    , getPrio
    , Hashtag
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.Time    (Date)
import Database.PostgreSQL.Simple.SqlQQ   (sql)

data Hashtag = Hashtag { getTodoId  :: !(Maybe Int)
                       , getHashtag :: !String
                       } deriving (Show, Eq)

data HashtagTodo = HashtagTodo { getHashtagTodoId :: !Int
                               , getHashtagHash   :: !String
                               , getTitle         :: !String
                               , getDueDate       :: !Date
                               , getPrio          :: !(Maybe Int)
                               }

instance FromRow Hashtag where
    fromRow = Hashtag <$> field -- the todo id
                      <*> field -- the hashtag string

instance FromRow HashtagTodo where
    fromRow = HashtagTodo <$> field -- the todo id
                          <*> field -- the hashtag string
                          <*> field -- the title
                          <*> field -- the due date
                          <*> field -- the prio value

allHashtags :: Connection -> IO [Hashtag]
allHashtags conn = query_ conn "select * from hashtags"

allHashtagsWithTodos :: Connection -> IO [HashtagTodo]
allHashtagsWithTodos conn = query_ conn q
                            where
                              q = [sql|
                                select todo_id, hashtag, title, due_date, prio
                                from hashtags h
                                join todos t
                                on t.id = h.todo_id |]

allHashtagsForTodo :: Connection -> Int -> IO [Hashtag]
allHashtagsForTodo conn tid = query conn q (Only tid)
                              where
                               q = [sql| select todo_id, hashtag
                                         from hashtags where todo_id = ? |]
