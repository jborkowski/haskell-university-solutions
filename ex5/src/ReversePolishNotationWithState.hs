module ReversePolishNotationWithState
  ( solveEquation
  , replaceAllResults
  , addErrorMsg
  ) where

import qualified Data.Map.Strict as Map
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Text.Read

type Result = String
type ResultMap = Map.Map String Result

type Digit = Double
 

solveEquation :: String -> Maybe Double
solveEquation = head . foldl fn [] . words
  where fn (Nothing:_) _ = [Nothing]
        fn (a:b:acc) "+" = (pure (+) <*> a <*> b) : acc
        fn (a:b:acc) "-" = (pure (-) <*> a <*> b) : acc
        fn (a:b:acc) "/" = (pure (/) <*> a <*> b) : acc
        fn (a:b:acc) "*" = (pure (*) <*> a <*> b) : acc
        fn acc item      = readMaybe item:acc

addErrorMsg :: Maybe a -> String -> Either String a
addErrorMsg mayA errorMsg = case mayA of
  Nothing -> Left errorMsg
  Just a  -> Right a

replaceRes :: [String] -> ResultMap -> [String]
replaceRes w m = map (\e -> if isPrefixOf "res" e then Map.findWithDefault e e m else e) w

replaceAllResults :: String -> ResultMap -> Either String [String]
replaceAllResults input resMap = 
    mapM (\i -> if isPrefixOf "res" i then resultLookup i resMap else Right i) . words $ input  

resultLookup :: String -> ResultMap -> Either String String
resultLookup resId resMap = case Map.lookup resId resMap of
  Nothing  -> Left ("Result " ++ show resId ++ "doesn't exist")
  Just res -> return res
