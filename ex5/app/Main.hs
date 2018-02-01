{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.List
import Control.Monad
import Control.Monad.State
import ReversePolishNotationWithState
import System.Console.Haskeline

main :: IO (a, Map.Map String String)
main = runStateT (forever appState) Map.empty

appState :: StateT (Map.Map String String) IO ()
appState = do
  --            <- lift (print "> ")
  line         <- lift getLine
  resultsState <- get -- get from state
  let result = do
        replaced <- replaceAllResults line resultsState
        result   <- addErrorMsg (solveEquation (unwords replaced)) "Can't calculate result of provided equation"
        return result
        
  case result of 
    Left msg  -> lift (print msg)
    Right res -> do
      let newKey = "res" ++ show (Map.size resultsState)
      put (Map.insert newKey (show res) resultsState)   
      lift (print (show res))

