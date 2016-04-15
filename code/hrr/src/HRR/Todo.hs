{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HRR.Todo
    ( -- Exports
      Todo(..)
    , PiTodo(..)
    , piTodo'
    , todoId'
    , dueDate'
    , title'
    , prio'
    , todo
    , tableOfTodo
    , insertTodo
    , insertQueryTodo
    , selectTodo
    , updateTodo
    ) where

import HRR.DataSource
import GHC.Int
       ( Int32 )
import Data.Time.Calendar
       ( Day )
import Database.HDBC.Query.TH
       ( makeRecordPersistableDefault )
import Database.Relational.Query


-- Keep in mind this will turn all the fields in the table into a
-- CamelCase
--
-- https://github.com/khibino/haskell-relational-record/blob/master/names-th/src/Language/Haskell/TH/Name/CamelCase.hs
--
-- Why they didn't use Text.Inflections.Camelize I still don't know
--

$(defineTable "public" "todo" [''Show])

instance Eq Todo where
    t1 == t2 = (todoId t1) == (todoId t2)

data PiTodo = PiTodo { piTodoTitle :: String
                     , piTodoDate :: Day
                     , piTodoPrio :: Maybe Int32
                     }

$(makeRecordPersistableDefault ''PiTodo)

piTodo' :: Pi Todo PiTodo
piTodo' = PiTodo |$| title'
                 |*| dueDate'
                 |*| prio'
