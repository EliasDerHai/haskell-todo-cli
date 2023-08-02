module Task where

data Task = Task
  { description :: String,
    completed :: Bool
  }

newTask :: String -> Task
newTask desc = Task {description = desc, completed = False}

formatTask :: Int -> Task -> String
formatTask index task =
  let checkBox = if completed task then "X" else " "
   in show index ++ ". [" ++ checkBox ++ "] " ++ description task

completeTask :: Int -> [Task] -> [Task]
completeTask n tasks =
  let (before, after) = splitAt (n - 1) tasks -- n-1 bc the user input starts from 1
      modifiedTask = case after of
        (t : ts) -> t {completed = True} : ts
        [] -> []
   in before ++ modifiedTask