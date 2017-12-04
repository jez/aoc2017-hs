module AoC.InverseCaptchaSpec where

import Test.Hspec

import AoC (sumEqualAdjacentPairs, sumEqualOppositePairs)

spec :: Spec
spec = do
  describe "sumEqualAdjacentPairs" $ do
    it "first matches second, third matches fourth" $ do
      (sumEqualAdjacentPairs "1122") `shouldBe` 3

    it "each digit matches the next" $ do
      (sumEqualAdjacentPairs "1111") `shouldBe` 4

    it "no digit matches the next" $ do
      (sumEqualAdjacentPairs "1234") `shouldBe` 0

    it "terminal 9 wraps and matches initial 9" $ do
      (sumEqualAdjacentPairs "91212129") `shouldBe` 9

  describe "sumEqualOppositePairs" $ do
    it "all digits match the one 2 ahead of it" $ do
      (sumEqualOppositePairs "1212") `shouldBe` 6

    it "every comparison is between a 1 and a 2" $ do
      (sumEqualOppositePairs "1221") `shouldBe` 0

    it "both 2's match each other" $ do
      (sumEqualOppositePairs "123425") `shouldBe` 4

    it "all 1's match each other" $ do
      (sumEqualOppositePairs "12131415") `shouldBe` 4
