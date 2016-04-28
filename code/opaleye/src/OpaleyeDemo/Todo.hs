{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- | See TodoManual.hs to avoid using TemplateHaskell, though you should be
-- | aware that this will make you write more boilerplate code
module OpaleyeDemo.Todo
      ( -- * Exports
        todoTable
      , todoQuery
      ) where

import Opaleye
import Data.Profunctor.Product

data Todo' i t d p = Todo' { _id :: i
                           , _title :: t
                           , _dueDate :: d
                           , _prio :: p
                           }
type TodoId = Int
type Prio   = Maybe Int
type Todo   = Todo' TodoId String Day Prio

type TodoColumn = Todo' ( Column PGInt4, Column PGText
                        , Column PGDate, Maybe (Column PGInt4) )


$(makeAdaptorAndInstance "pTodo" ''Todo')

todoTable
  :: Table (Column PGInt4, Column PGText, Column PGDate, Maybe (Column PGInt4))
           (Column PGInt4, Column PGText, Column PGDate, Column PGInt4)
todoTable = Table "todos" (p4 ( required "id"
                              , required "title"
                              , required "due_date"
                              , optional "prio" ))

todoQuery :: Query (Column PGInt4, Column PGText, Column PGDate, Column PGInt4)
todoQuery = queryTable todoTable

