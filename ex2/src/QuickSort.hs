module QuickSort
  (quicksort
  ) where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
    where
      smaller = [s | s <- xs, s <= x]
      larger  = [g | g <- xs, g > x ]

