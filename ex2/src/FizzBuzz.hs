module FizzBuzz
  (fizzbuzz
  ) where

fizzbuzz :: Int -> String
fizzbuzz x
    | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
    | x `mod` 5 == 0 = "Buzz"
    | x `mod` 3 == 0 = "Fizz"
    | otherwise = show x