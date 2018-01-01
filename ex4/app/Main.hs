module Main where

import Control.Monad
import ReversePolishNotation
import Data.Char

promptLn :: String -> IO String
promptLn prompt = do
  putStr prompt
  getLine

getResult :: Maybe Double -> String -> String
getResult (Just a) _ = show a
getResult Nothing expression = "Could not evaluate '" ++ expression ++ "'"

main = forever $ do 
  line <- promptLn "> "
  putStrLn $ (getResult (solveM line) line)