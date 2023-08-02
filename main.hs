module Main where

import Command (executeCommand, getCommand)
import System.Process
import Task (Task)

clearScreen :: IO ()
clearScreen = system "clear || cls" >> return ()

main :: IO ()
main = loop []

loop :: [Task] -> IO ()
loop tasks = do
  putStrLn "Please enter your command:"
  line <- getLine
  case getCommand line of
    Just (command, arg) -> do
      clearScreen
      newTasks <- executeCommand command arg tasks
      loop newTasks
    Nothing -> do
      clearScreen
      putStrLn "Invalid command"
  loop tasks
