module Main where

import Runner
import qualified PerformanceTodo as Performance
import qualified CorrectTodo as Correct

main :: IO ()
main = do
  n <- (read :: String -> Int) <$> getLine
  -- Switch to the performant variant for a large number of commands
  if n < 1024 then Correct.runTodoList $ runNQueries n else Performance.runTodoList $ runNQueries n
