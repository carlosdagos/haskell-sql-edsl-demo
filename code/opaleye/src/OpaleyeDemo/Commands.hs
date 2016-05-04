{-# LANGUAGE OverloadedStrings #-}

module OpaleyeDemo.Commands
    ( -- * Exports
      runAndPrintCommand
      -- * Commands
    , Command(..)
     -- * Flags
    , Flag(..)
    ) where

import qualified Data.ByteString.Char8           as B (pack, split, unpack)
import           Data.List                       (intercalate)
import           Data.Maybe                      (fromJust, isJust, isNothing)
import           Data.Time.Calendar              (fromGregorian)
import           Database.PostgreSQL.Simple      (Connection, Only (..))
import           Database.PostgreSQL.Simple.Time (Date, parseDate)
import qualified OpaleyeDemo.Hashtag             as H
import qualified OpaleyeDemo.Todo                as T
import           System.Exit
import           System.IO

--------------------------------------------------------------------------------
-- | Commands available for the application
data Command
    = List                  -- ^ "list" command
    | Find Int              -- ^ "find" command
    | Add String            -- ^ "add" command
    | Complete Int          -- ^ "complete" command
    deriving (Show)

--------------------------------------------------------------------------------
-- | Flags available for the app
data Flag
    = DueBy String          -- ^ --due-by
    | Late                  -- ^ --late
    | WithHashtags          -- ^ --with-hashtags
    | SearchHashtag String  -- ^ --hashtags
    | OrderByPriority       -- ^ --order-by-priority
    | SetPriority String    -- ^ --priority
    | Help                  -- ^ --help
    | Version               -- ^ --version
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Main command runner
runAndPrintCommand :: Connection -> Command -> [Flag] -> IO ()
runAndPrintCommand = undefined
