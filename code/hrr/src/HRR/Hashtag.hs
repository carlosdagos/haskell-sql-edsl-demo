{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HRR.Hashtag
    ( -- Exports
      Hashtag(..)
    , todoId'
    , hashtagStr'
    , tableOfHashtag
    , insertHashtag
    , insertQueryHashtag
    , selectHashtag
    , updateHashtag
    ) where

import HRR.DataSource

-- Make sure the table is defined first, otherwise you'll get some very
-- bizzare errors
$(defineTable "public" "hashtag" [''Eq, ''Show])
