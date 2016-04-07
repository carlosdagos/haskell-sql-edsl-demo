module Simple.Hashtag
    ( -- * Exports
      Hashtag
    ) where

import Simple.Todo

data Hashtag = Hashtag { getTodoId  :: !(Maybe Int)
                       , getHashtag :: !String
                       } deriving (Show, Eq)
