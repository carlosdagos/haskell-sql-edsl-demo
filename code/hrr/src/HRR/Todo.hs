{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HRR.Todo
    ( -- Exports
      Todo(..)
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
