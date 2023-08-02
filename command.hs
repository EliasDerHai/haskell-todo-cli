module Command where

import Data.ByteString.Char8 (split)
import Data.Char (toLower)
import System.Exit (exitSuccess)
import Task
import Text.Read (readMaybe)

data CommandType = Add | View | Remove | Complete | Exit | Help deriving (Show, Bounded, Enum)

allCommands :: [CommandType]
allCommands = [minBound .. maxBound]

getCommand :: String -> Maybe (CommandType, String)
getCommand input =
  case words input of
    (cmd : args) -> do
      command <- lowercaseToCommandType cmd
      return (command, unwords args)
    _ -> Nothing

lowercaseToCommandType :: String -> Maybe CommandType
lowercaseToCommandType s = case map toLower s of
  "--add" -> Just Add
  "-a" -> Just Add
  "--view" -> Just View
  "-v" -> Just View
  "--remove" -> Just Remove
  "-r" -> Just Remove
  "--complete" -> Just Complete
  "-c" -> Just Complete
  "--exit" -> Just Exit
  "-e" -> Just Exit
  "help" -> Just Help
  "-h" -> Just Help
  "--help" -> Just Help
  _ -> Nothing

executeCommand :: CommandType -> String -> [Task] -> IO [Task]
executeCommand command arg tasks =
  case command of
    Add -> do
      let newTasks = tasks ++ [newTask arg]
      printAllTasks newTasks
      return newTasks
    View -> do
      printAllTasks tasks
      return tasks
    Complete -> do
      case readMaybe arg of
        Just n -> do
          let newTasks = completeTask n tasks
          putStrLn ("Completed todo " ++ show n)
          printAllTasks newTasks
          return newTasks
        Nothing -> do
          putStrLn "Could not read arg as number"
          return tasks
    Remove -> do
      case readMaybe arg of
        Just n -> do
          let newTasks = removeNth (n - 1) tasks
          putStrLn ("Removed todo " ++ show n)
          printAllTasks newTasks
          return newTasks
        Nothing -> do
          putStrLn "Could not read arg as number"
          return tasks
    Exit -> do
      putStrLn "This program will now exit."
      exitSuccess
    Help -> do
      printAllCommands
      return tasks

printAllCommands :: IO ()
printAllCommands = do
  putStrLn "Available commands:"
  mapM_ print allCommands
  putStrLn "Call like eg. Add: --add 'My todo' or -a'My todo'"

printAllTasks :: [Task] -> IO ()
printAllTasks tasks = do
  if null tasks
    then putStrLn "No todos"
    else do
      putStrLn "Todos:"
      mapM_ putStrLn (zipWith formatTask [1 ..] tasks)

removeNth :: Int -> [a] -> [a]
removeNth n xs = let (ys, zs) = splitAt n xs in ys ++ drop 1 zs
