{-# LANGUAGE OverloadedStrings #-}

import Simple.Todo                 (Todo, TodoId, Prio, allTodos)
import Simple.Hashtag              (Hashtag)
import Database.PostgreSQL.Simple

main :: IO ()
main = do
    conn     <- connect defaultConnectInfo
    [Only i] <- query_ conn "select id, "

    putStrLn i
