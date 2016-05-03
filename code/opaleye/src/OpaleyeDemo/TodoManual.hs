{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpaleyeDemo.TodoManual
      ( -- * Exports
        todoTable
      , todoQuery
      ) where

import Opaleye
import Data.Profunctor.Product

todoTable
  :: Table (Column PGInt4, Column PGText, Column PGDate, Maybe (Column PGInt4))
           (Column PGInt4, Column PGText, Column PGDate, Column PGInt4)
todoTable = Table "todos" (p4 ( required "id"
                              , required "title"
                              , required "due_date"
                              , optional "prio" ))

todoQuery :: Query (Column PGInt4, Column PGText, Column PGDate, Column PGInt4)
todoQuery = queryTable todoTable


