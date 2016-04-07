module Simple.Hashtag
    ( -- * Exports
      Hashtag
    ) where

import Simple.Todo (TodoId)

data Hashtag = Hashtag { getTodoId  :: !TodoId
                       , getHashtag :: String
                       } deriving (Show, Eq)
