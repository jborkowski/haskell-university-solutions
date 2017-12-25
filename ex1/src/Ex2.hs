module Ex2
  (greeting
  ) where

greeting :: IO()
greeting = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn $ "Nice to meet you " ++ name