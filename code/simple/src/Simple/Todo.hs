{-# LANGUAGE OverloadedStrings #-}

module Simple.Todo
    ( -- * Exports
      TodoId
    , allTodos
    , Prio
    , Todo
    ) where

import Database.PostgreSQL.Simple         (Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)

type TodoId = Int
type Prio   = Maybe Int

data Todo = Todo { getId       :: !TodoId
                 , getTitle    :: !String
                 , getDueDate  :: !String
                 , getPriority :: !Prio
                 } deriving (Show)

instance FromRow Todo where
    fromRow = Todo <$> field -- id
                   <*> field -- title
                   <*> field -- due date
                   <*> field -- prio

allTodosQuery :: Query
allTodosQuery = "select id, title, due_date, prio from todos"

findTodoQuery :: Query
findTodoQuery = "select id, title, due_date, prio from todos where id = ?"

deleteTodoQuery :: Query
deleteTodoQuery = "delete from todos where id = ?"

addTodoQuery :: Query
addTodoQuery = "insert into todos (title, due_date, priority) values (?, ?, ?)"

allTodosByDateQuery :: Query
allTodosByDateQuery = "select id, title, due_date, prio from todos where due_date = ?"

allTodosByPrioQuery :: Query
allTodosByPrioQuery = "select id, title, due_date, prio from todos order by prio"

allLateTodosQuery :: Query
allLateTodosQuery = "select id, title, due_date, prio from todos where due_date < current_date"
