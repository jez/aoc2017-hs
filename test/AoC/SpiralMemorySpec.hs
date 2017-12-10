module AoC.SpiralMemorySpec where

import Test.Hspec

import AoC.SpiralMemory

spec :: Spec
spec = do
  describe "manhattanSpiralCoord" $ do
    it "handles the center (special case)" $ do
      manhattanSpiralCoord 1 `shouldBe` ( 0,  0)

    it "handles the first ring" $ do
      manhattanSpiralCoord 2 `shouldBe` ( 1,  0)
      manhattanSpiralCoord 3 `shouldBe` ( 1,  1)
      manhattanSpiralCoord 4 `shouldBe` ( 0,  1)
      manhattanSpiralCoord 5 `shouldBe` (-1,  1)
      manhattanSpiralCoord 6 `shouldBe` (-1,  0)
      manhattanSpiralCoord 7 `shouldBe` (-1, -1)
      manhattanSpiralCoord 8 `shouldBe` ( 0, -1)
      manhattanSpiralCoord 9 `shouldBe` ( 1, -1)

    it "handles the second ring" $ do
      manhattanSpiralCoord 10 `shouldBe` ( 2, -1)
      manhattanSpiralCoord 11 `shouldBe` ( 2,  0)
      manhattanSpiralCoord 12 `shouldBe` ( 2,  1)
      manhattanSpiralCoord 13 `shouldBe` ( 2,  2)
      manhattanSpiralCoord 14 `shouldBe` ( 1,  2)
      manhattanSpiralCoord 15 `shouldBe` ( 0,  2)
      manhattanSpiralCoord 16 `shouldBe` (-1,  2)
      manhattanSpiralCoord 17 `shouldBe` (-2,  2)
      manhattanSpiralCoord 18 `shouldBe` (-2,  1)
      manhattanSpiralCoord 19 `shouldBe` (-2,  0)
      manhattanSpiralCoord 20 `shouldBe` (-2, -1)
      manhattanSpiralCoord 21 `shouldBe` (-2, -2)
      manhattanSpiralCoord 22 `shouldBe` (-1, -2)
      manhattanSpiralCoord 23 `shouldBe` ( 0, -2)
      manhattanSpiralCoord 24 `shouldBe` ( 1, -2)
      manhattanSpiralCoord 25 `shouldBe` ( 2, -2)

  describe "manhattanSpiralDist" $ do
    it "handles the center (special case)" $ do
      manhattanSpiralDist 1 `shouldBe` 0

    it "handles the first ring" $ do
      manhattanSpiralDist 2 `shouldBe` 1
      manhattanSpiralDist 3 `shouldBe` 2
      manhattanSpiralDist 4 `shouldBe` 1
      manhattanSpiralDist 5 `shouldBe` 2
      manhattanSpiralDist 6 `shouldBe` 1
      manhattanSpiralDist 7 `shouldBe` 2
      manhattanSpiralDist 8 `shouldBe` 1
      manhattanSpiralDist 9 `shouldBe` 2

    it "handles the second ring" $ do
      manhattanSpiralDist 10 `shouldBe` 3
      manhattanSpiralDist 11 `shouldBe` 2
      manhattanSpiralDist 12 `shouldBe` 3
      manhattanSpiralDist 13 `shouldBe` 4
      manhattanSpiralDist 14 `shouldBe` 3
      manhattanSpiralDist 15 `shouldBe` 2
      manhattanSpiralDist 16 `shouldBe` 3
      manhattanSpiralDist 17 `shouldBe` 4
      manhattanSpiralDist 18 `shouldBe` 3
      manhattanSpiralDist 19 `shouldBe` 2
      manhattanSpiralDist 20 `shouldBe` 3
      manhattanSpiralDist 21 `shouldBe` 4
      manhattanSpiralDist 22 `shouldBe` 3
      manhattanSpiralDist 23 `shouldBe` 2
      manhattanSpiralDist 24 `shouldBe` 3
      manhattanSpiralDist 25 `shouldBe` 4

  describe "firstSpiralGreater" $ do
    it "handles implicit 0 (center starts with 1)" $ do
      firstSpiralGreater 0  `shouldBe` 1

    it "handles the first ring" $ do
      firstSpiralGreater 1  `shouldBe` 2
      firstSpiralGreater 2  `shouldBe` 4
      firstSpiralGreater 4  `shouldBe` 5
      firstSpiralGreater 5  `shouldBe` 10
      firstSpiralGreater 10 `shouldBe` 11
      firstSpiralGreater 11 `shouldBe` 23
      firstSpiralGreater 23 `shouldBe` 25

    it "handles the second ring" $ do
      firstSpiralGreater 25 `shouldBe` 26
      firstSpiralGreater 26 `shouldBe` 54
      firstSpiralGreater 54 `shouldBe` 57
      firstSpiralGreater 57 `shouldBe` 59
      firstSpiralGreater 59 `shouldBe` 122
      firstSpiralGreater 122 `shouldBe` 133
      firstSpiralGreater 133 `shouldBe` 142
      firstSpiralGreater 142 `shouldBe` 147
      firstSpiralGreater 147 `shouldBe` 304
      firstSpiralGreater 304 `shouldBe` 330
      firstSpiralGreater 330 `shouldBe` 351
      firstSpiralGreater 351 `shouldBe` 362
      firstSpiralGreater 362 `shouldBe` 747
      firstSpiralGreater 747 `shouldBe` 806
