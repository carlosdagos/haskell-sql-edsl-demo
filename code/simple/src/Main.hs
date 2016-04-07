{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple

type TodoId = Int
type Prio   = Maybe Int

data Todo = Todo { getId       :: !TodoId
                 , getTitle    :: !String
                 , getDueDate  :: !String
                 , getPriority :: !Prio
                 } deriving (Show)

data Hashtag = Hashtag { getTodoId  :: !TodoId
                       , getHashtag :: String
                       } deriving (Show, Eq)

instance Eq Todo where
    x == y = getId x == getId y

main :: IO ()
main = do
    conn     <- connect defaultConnectInfo
    [Only i] <- query_ conn "select 2+2"

    putStrLn i
