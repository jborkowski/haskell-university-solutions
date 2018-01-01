module ReversePolishNotation
  ( solveM
  ) where

import Data.List.Split (splitOn)
import Text.Read
import Control.Applicative

solveM :: String -> Maybe Double
solveM = head . foldl fn [] . splitOn " "
  where fn (Nothing:_) _ = [Nothing]
        fn (a:b:acc) "+" = (pure (+) <*> a <*> b) : acc
        fn (a:b:acc) "-" = (pure (-) <*> a <*> b) : acc
        fn (a:b:acc) "/" = (pure (/) <*> a <*> b) : acc
        fn (a:b:acc) "*" = (pure (*) <*> a <*> b) : acc
        fn acc item      = readMaybe item:acc
         


