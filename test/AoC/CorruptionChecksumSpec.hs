module AoC.CorruptionChecksumSpec where

import Test.Hspec

import AoC (sumOfDifferences, sumOfDivisibles)

spec :: Spec
spec = do
  describe "sumOfDifferences" $ do
    it "9 - 1;  7 - 3;  8 - 2" $ do
      sumOfDifferences [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]] `shouldBe` 18

  describe "sumOfDivisibles" $ do
    it "8 / 2;  9 / 3;  6 / 3" $ do
      sumOfDivisibles [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]] `shouldBe` 9

