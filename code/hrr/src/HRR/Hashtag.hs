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

$(defineTable "public" "hashtag" [''Eq, ''Show])
