module Main where

import System.Environment
import TaskAssigner
import Text.Read


main :: IO ()
main = do
  args <- getArgs
  case args of
    [tasks]    -> run tasks assignMemo
    [tasks, m] -> parseMethodAndRun tasks m
    _          -> printHelp

parseMethodAndRun :: String -> String -> IO ()
parseMethodAndRun tasks m =
  case parseMethod m of
    Just f -> run tasks f
    _      -> printHelp
  where 
    parseMethod m' = 
      case m' of 
        "-a" -> Just assignMemo
        "-b" -> Just assignNonMemo
        "-c" -> Just assignDFS
        "-d" -> Just assignDFS'
        _    -> Nothing

run :: String -> TaskAssigner -> IO ()
run tasks f =
  case readMaybe tasks of
    Just ts -> runAssignment ts f
    _       -> printHelp

runAssignment :: [Int] -> TaskAssigner -> IO ()
runAssignment ts f = do
  let r = f (Task <$> ts)
  case r of
    Just assignment -> printSolution assignment
    _               -> putStrLn "No solution found"

printHelp :: IO ()
printHelp = do
  putStrLn "Invalid input. Usage:"
  putStrLn "    docker-run.sh <tasks> [method]"
  putStrLn ""
  putStrLn "    tasks - a string holding the list of tasks (positive integers)"
  putStrLn "            for example: '[5, 4, 1, 2, 7, 8, 3]'"
  putStrLn ""
  putStrLn "    method - (optional) algorithm used to solve. One of:"
  putStrLn $ "        -a (default) : Algorithm that memoizes subsets of tasks that sum to a particular number." 
        ++ " (Note: memory usage is proportional to the sum of tasks)"
  putStrLn "        -b : Algorithm above without memoization"
  putStrLn "        -c : Depth first search with pruning"
  putStrLn "        -d : Naive recursive depth first search algorithm"

printSolution :: TaskAssignment -> IO ()
printSolution (TaskAssignment c1 c2 c3) = do
  putStrLn   "Solution Found:"
  putStrLn $ "    child1: " ++ showChild c1
  putStrLn $ "    child2: " ++ showChild c2
  putStrLn $ "    child3: " ++ showChild c3

showChild :: [Task] -> String
showChild = show . fmap points 