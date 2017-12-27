module QuicksortSpec
  (spec
  ) where

import Test.Hspec
import Test.QuickCheck
import QuickSort

spec :: Spec
spec = do
  describe "quicksort" $ do
    it "returns the sorted array when given a unsorted array" $
      quicksort [ 9, 8, 7, 5, 3, 1 ] `shouldBe` [ 1, 3, 5, 7, 8, 9 ]
