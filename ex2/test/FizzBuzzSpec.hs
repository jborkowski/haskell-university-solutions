module FizzBuzzSpec
  (spec
  ) where

import Test.Hspec
import Test.QuickCheck
import FizzBuzz

spec :: Spec
spec = do
  describe "fizzbuzz" $ do
    it "should return 'FizzBuzz' for number divided by '5' and '3'" $
      fizzbuzz 15 `shouldBe` "FizzBuzz"
    it "should return 'Fizz' for number diviced by '3'" $
      fizzbuzz 6 `shouldBe` "Fizz"
    it "should return 'Buzz' for number diviced by '5'" $
      fizzbuzz 10 `shouldBe` "Buzz"
    it "should return number for other cases" $
      fizzbuzz 89 `shouldBe` "89"
