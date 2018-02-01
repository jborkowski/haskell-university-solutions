module ReversePolishNotationWithState
  ( solveEquation
  ) where

import Control.Applicative
import Text.Read

solveEquation :: String -> Maybe Double
solveEquation = head . foldl fn [] . words
  where fn (Nothing:_) _ = [Nothing]
        fn (a:b:acc) "+" = (pure (+) <*> a <*> b) : acc
        fn (a:b:acc) "-" = (pure (-) <*> a <*> b) : acc
        fn (a:b:acc) "/" = (pure (/) <*> a <*> b) : acc
        fn (a:b:acc) "*" = (pure (*) <*> a <*> b) : acc
        fn acc item      = readMaybe item:acc
         
