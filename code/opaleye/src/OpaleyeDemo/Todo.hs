{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- | See TodoManual.hs to avoid using TemplateHaskell, though you should be
-- | aware that this will make you write more boilerplate code
module OpaleyeDemo.Todo where

import Data.Int
       ( Int64 )
import Data.Time.Calendar
       ( Day )
import Control.Arrow
       ( returnA )
import Opaleye
       ( Table(..), Query, Column, Nullable, PGInt4, PGText, PGDate
       , runInsertReturning, runDelete, queryTable, required, optional
       , restrict, (.==), (.===), pgInt4 )
import Data.Profunctor.Product.TH
       ( makeAdaptorAndInstance )
import Database.PostgreSQL.Simple
       ( Connection )

--------------------------------------------------------------------------------
-- | Todo Id
data TodoId' a = TodoId { todoId :: a } deriving Show
makeAdaptorAndInstance "pTodoId" ''TodoId'

type TodoId = TodoId' Int
type TodoIdColumn = TodoId' (Column PGInt4)
type TodoIdColumnMaybe = TodoId' (Maybe (Column PGInt4))

--------------------------------------------------------------------------------
-- | Priority indicator
data Prio' a = Prio { prio :: a } deriving Show
makeAdaptorAndInstance "pPrio" ''Prio'

type Prio = Prio' (Maybe Int)
type PrioColumn = Prio' (Column (Nullable PGInt4))

--------------------------------------------------------------------------------
-- | Todo declaration
data Todo' i t d p = Todo { _id :: i
                          , _title :: t
                          , _dueDate :: d
                          , _prio :: p
                          } deriving Show
makeAdaptorAndInstance "pTodo" ''Todo'

--------------------------------------------------------------------------------
-- | Todo Columns
type TodoColumns
  = Todo' TodoIdColumn (Column PGText) (Column PGDate) PrioColumn
type TodoInsertColumns
  = Todo' TodoIdColumnMaybe (Column PGText) (Column PGDate) PrioColumn
type Todo
  = Todo' TodoId String Day Prio

--------------------------------------------------------------------------------
-- | Functions
todoTable :: Table TodoInsertColumns TodoColumns
todoTable = Table "todos" $ pTodo Todo
    { _id      = pTodoId . TodoId $ optional "id"
    , _title   = required "title"
    , _dueDate = required "due_date"
    , _prio    = pPrio . Prio $ required "prio"
    }

todoQuery :: Query TodoColumns
todoQuery = queryTable todoTable

selectTodo :: TodoId -> Query TodoColumns
selectTodo tid = proc () -> do
    todos <- todoQuery -< ()
    restrict -< todoId (_id todos) .== pgInt4 (todoId tid)
    returnA  -< todos

insertTodo :: Connection -> TodoInsertColumns -> IO TodoId
insertTodo conn t = fmap head (runInsertReturning conn todoTable t _id)

deleteTodo :: Connection -> TodoId -> IO Int64
deleteTodo conn tid = runDelete conn todoTable
                      (\t -> todoId (_id t) .=== (pgInt4 . todoId) tid)
