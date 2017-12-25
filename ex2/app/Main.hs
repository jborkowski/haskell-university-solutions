module Main where

import QuickSort
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print . quicksort . argsToIntArray $ args

argsToIntArray :: [String] -> [Int]
argsToIntArray = map read
