module AoC.CorruptionChecksum
  ( sumOfDifferences
  , sumOfDivisibles
  ) where

differences :: Integral a => [a] -> a
differences row = maximum row - minimum row

divisibles :: Integral a => [a] -> a
divisibles row =
  -- Head is safe here; we're guaranteed that the solution exists in every row
  head [x `div` y | x <- row, y <- row, x /= y, x `mod` y == 0]

corruptionChecksum :: Integral a => ([a] -> a) -> [[a]] -> a
corruptionChecksum rowFn spreadsheet =
  sum $ map rowFn spreadsheet

sumOfDifferences :: [[Int]] -> Int
sumOfDifferences = corruptionChecksum differences

sumOfDivisibles :: [[Int]] -> Int
sumOfDivisibles = corruptionChecksum divisibles
