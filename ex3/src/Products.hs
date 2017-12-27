module Products 
  (Fruit(..),
  checkout
  ) where

import Data.List

data Fruit = Apple | Orange deriving (Eq, Ord, Show)

checkout :: [Fruit] -> Float
checkout ls = sumBasket . groupFruit $ ls

sumBasket :: [(Fruit,Int)] -> Float
sumBasket [] = 0
sumBasket ((Apple, n) : xs) =
  (calc 0.60 $ promotion n 1 1) + sumBasket xs
  sumBasket ((Orange, n) : xs) =
  (calc 0.25 $ promotion n 3 1) + sumBasket xs

calc :: Float -> Int -> Float
calc p n = p * (fromIntegral n)

promotion :: Int -> Int -> Int -> Int
promotion n max prom
    | n > max   = n - prom
    | otherwise = n

groupFruit :: [Fruit] -> [(Fruit,Int)]
groupFruit ls  = 
  map (\l@(x:xs) -> (x, length l)) . group . sort $ ls