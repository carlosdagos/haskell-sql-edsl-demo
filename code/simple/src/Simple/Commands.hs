{-# LANGUAGE OverloadedStrings #-}

module Simple.Commands
    ( -- * Exports
      runAndPrintCommand
      -- * Commands
    , Command(..)
     -- * Flags
    , Flag(..)
    ) where

--------------------------------------------------------------------------------
-- | Commands available for the application
data Command
    = List                  -- list
    | Find Int              -- find
    | Add String            -- add
    | Complete Int          -- complete
    deriving (Show)

--------------------------------------------------------------------------------
-- | Flags available for the app
data Flag
    = DueBy String          --due-by
    | Late                  --late
    | WithHashtags          --with-hashtags
    | SearchHashtag String  --hashtags
    | OrderByPriority       --order-by-priority
    | SetPriority String    --priority
    | Help                  --help
    | Version               --version
    deriving (Show)

--------------------------------------------------------------------------------
-- | Main command runner
runAndPrintCommand :: Command -> [Flag] -> IO ()
runAndPrintCommand (Find x) flags     = runFindCommand x flags
runAndPrintCommand (Add  x) flags     = runAddCommand x flags
runAndPrintCommand (Complete x) flags = runCompleteCommand x flags
runAndPrintCommand List flags         = runListCommand flags

runFindCommand :: Int -> [Flag] -> IO ()
runFindCommand = undefined

runAddCommand :: String -> [Flag] -> IO ()
runAddCommand = undefined

runCompleteCommand :: Int -> [Flag] -> IO ()
runCompleteCommand = undefined

runListCommand :: [Flag] -> IO ()
runListCommand = undefined
