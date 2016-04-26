module HRR.Reports
    ( -- * Exports
      runReports
    ) where

import           Data.Int
                 ( Int32 )
import           Database.Relational.Query
import           Database.HDBC
                 ( IConnection )
import           Database.HDBC.Record
                 ( runQuery )
import qualified HRR.Todo                   as T
import qualified HRR.Hashtag                as H
import qualified HRR.ConnectionHelpers      as C

runReports :: (IConnection conn) => conn -> IO ()
runReports conn = do
    C.run conn () todosWithoutHashtags
    C.run conn () countLateTodos
    C.run conn () countFutureTodos
    C.run conn () hashtagsWithTodos

todosWithoutHashtags :: Relation () (Int, String)
todosWithoutHashtags = undefined

countLateTodos :: Relation () (Int, String)
countLateTodos = undefined

countFutureTodos :: Relation () (Int, String)
countFutureTodos = undefined

hashtagsWithTodos :: Relation () (Int, String)
hashtagsWithTodos = undefined

