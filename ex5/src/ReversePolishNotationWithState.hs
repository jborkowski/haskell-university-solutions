module ReversePolishNotationWithState
  ( solveM
  ) where

import Control.Applicative
import Text.Read

solveM :: String -> Maybe Double
solveM = head . foldl fn [] . words
  where fn (Nothing:_) _ = [Nothing]
        fn (a:b:acc) "+" = (pure (+) <*> a <*> b) : acc
        fn (a:b:acc) "-" = (pure (-) <*> a <*> b) : acc
        fn (a:b:acc) "/" = (pure (/) <*> a <*> b) : acc
        fn (a:b:acc) "*" = (pure (*) <*> a <*> b) : acc
        fn acc item      = readMaybe item:acc
         
