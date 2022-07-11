{-# LANGUAGE BlockArguments #-}

module Console
( start
, consoleLoop
, commandExit
, Command (..),
) where


import Control.Monad (when)
import Data.List (find)
import Debug.Trace
import System.IO


start :: [Command] -> IO ()
start cmds = do
  let cmdList = cmds ++ [commandExit]
  consoleLoop cmdList
  where 
    createHelp = [] 


consoleLoop :: [Command] -> IO ()
consoleLoop cmds = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let command = words input
  when (null command) do
    -- empty case, just enter
    start cmds
  let r = find (\a -> head command == cmdName a) cmds
  case r of
    Just c -> do
      ret <- cmdIO c $ tail command
      case ret of
        0 -> consoleLoop cmds
        1 -> return ()
    Nothing -> do 
      putStrLn $ "Command \"" ++ head command ++ "\" not found. Try \"help\" to find all avalidable commands."
      consoleLoop cmds


data Command = Command
  { cmdName :: String,
    cmdDescription :: String,
    cmdArgs :: [String],
    cmdIO :: [String] -> IO Int
  }

commandExit :: Command
commandExit =
  Command
    { cmdName = "exit",
      cmdDescription = "Exit the console",
      cmdArgs = [],
      cmdIO = \_ -> do
        putStrLn "Are you sure? y/n"
        answer <- getLine
        return case answer of
          "y" -> 1
          "n" -> 0
    }
