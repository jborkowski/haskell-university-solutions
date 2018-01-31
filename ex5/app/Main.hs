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
  line    <- lift getLine
  results <- get -- get from state
  let replacedRes = replaceRes (words line) results
  _ <- lift (print replacedRes) -- debug only
  let Just result = solveM (unwords replacedRes)
  let size   = Map.size results
  let newKey = "res" ++ show size
  put (Map.insert newKey (show result) results)
  state <- get
  lift (print state) -- debug only
  lift (print result)
  -- TODO: parser - check that imput not waiting for result from state [DONE],
  -- TODO: add result to the state [DONE],
  -- TODO: add error handling - parser, error calculation, and messages
  -- TODO: clean up code! names refactor

replaceRes :: [String] -> (Map.Map String String) -> [String]
replaceRes w m = map (\e -> if isPrefixOf "res" e then Map.findWithDefault e e m else e) w

