{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.List
import Control.Monad
import Control.Monad.State
import ReversePolishNotationWithState

main :: IO (a, Map.Map String String)
main = runStateT (forever appState) Map.empty

appState :: StateT (Map.Map String String) IO ()
appState = do
  line         <- lift getLine
  resultsState <- get -- get from state
  let replacedS =  replaceAllResults line resultsState
  lift (print replacedS) -- debug only
  let result = do
        replaced <- replaceAllResults line resultsState
        result   <- addErrorMsg (solveEquation (unwords replaced)) "Can't calculate result of provided equation"
        return result
  
  case result of 
    Left msg  -> lift (print msg)
    Right res -> do
      let newKey = "res" ++ show (Map.size resultsState)
      put (Map.insert newKey (show result) resultsState)   
      lift (print (show res))

  -- TODO: parser - check that imput not waiting for result from state [DONE],
  -- TODO: add result to the state [DONE],
  -- TODO: add error handling - parser, error calculation, and messages[INPROGRESS]
  -- TODO: clean up code! names refactor

type Result = String
type ResultMap = Map.Map String Result

addErrorMsg :: Maybe a -> String -> Either String a
addErrorMsg mayA errorMsg = case mayA of
  Nothing -> Left errorMsg
  Just a  -> Right a

replaceRes :: [String] -> ResultMap -> [String]
replaceRes w m = map (\e -> if isPrefixOf "res" e then Map.findWithDefault e e m else e) w

replaceAllResults :: String -> ResultMap -> Either String [String]
replaceAllResults input resMap = 
    -- mapM (\i -> if isPrefixOf "res" i then resultLookup i resMap else Right i) . words $ input
    traverse fn . words $ input
    where fn i | isPrefixOf "res" i = resultLookup i resMap
               | otherwise          = Right i   

resultLookup :: String -> ResultMap -> Either String String
resultLookup resId resMap = case Map.lookup resId resMap of
  Nothing  -> Left ("Result " ++ show resId ++ "doesn't exist")
  Just res -> Right res
