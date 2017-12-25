import Test.QuickCheck
import QuickSort

prop_qsort :: (Ord a) => [a] -> [a] -> Bool
prop_qsort xs ys = quicksort xs == ys 

main = quickCheck prop_qsort