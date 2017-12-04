module AoC.InverseCaptcha
  ( sumEqualAdjacentPairs
  , sumEqualOppositePairs
  ) where

import Data.Char (digitToInt)
import Data.Function ((&))

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs digits = zip digits (tail $ cycle digits)

oppositePairs :: [a] -> [(a, a)]
oppositePairs digits = zip digits (drop mid $ cycle digits)
  where mid = length digits `div` 2

inverseCaptcha :: ([Int] -> [(Int, Int)]) -> String -> Int
inverseCaptcha pairGen captcha =
  captcha
  & map digitToInt
  & pairGen
  & filter (uncurry (==))
  & map fst
  & sum

sumEqualAdjacentPairs :: String -> Int
sumEqualAdjacentPairs = inverseCaptcha adjacentPairs

sumEqualOppositePairs :: String -> Int
sumEqualOppositePairs = inverseCaptcha oppositePairs
