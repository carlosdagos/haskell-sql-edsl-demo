{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HRR.Hashtag
    ( -- Exports
      Hashtag(..)
    , hashtag
    , todoId'
    , hashtagStr'
    , tableOfHashtag
    , insertHashtag
    , insertQueryHashtag
    , selectHashtag
    , updateHashtag
    , hashtagsForTodo
    ) where

import Data.Int
       ( Int32 )
import HRR.DataSource
       ( defineTable )
import Database.Relational.Query
       ( relation', placeholder, wheres, query
       , (.=.), (!), value, Relation(..) )

-- Make sure the table is defined first, otherwise you'll get some very
-- bizzare errors
$(defineTable "public" "hashtag" [''Eq, ''Show])

--hashtagsForTodo :: Relation Int32 Hashtag
hashtagsForTodo = relation' . placeholder $ \ph -> do
    h <- query hashtag
    wheres $ h ! todoId' .=. ph
    return h
