module AoC.SpiralMemory
  ( manhattanSpiralCoord
  , manhattanSpiralDist
  , firstSpiralGreater
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

isqrt :: Integer -> Integer
isqrt = (floor :: Double -> Integer) . sqrt . fromIntegral

lookup' :: Ord k => Map.Map k a -> k -> Maybe a
lookup' = flip Map.lookup

-- The result is the greatest odd square root smaller than 'n'.
greatestOddSqrt :: Integer -> Integer
greatestOddSqrt n =
  let root = isqrt n
   in if odd root then root else root - 1


-- Our goal is to find the ring that bounds the current point,
-- figure out which edge of that ring the current point is in,
-- and those two pieces of information to reconstruct the
-- coordinate for the current point in the spiral.
--
--      - - - - -
--      |       |
--      |   ·   |
--      |      →|
--      - - - - -
--
-- The '→' is where the next spiral starts, so it's where our offset starts.
--
-- Note also that each concentric square in the spiral is an odd square.


type Coordinate = (Integer, Integer)

offsetToCoord :: Integer -> Integer -> Coordinate
offsetToCoord edge offset
  | offset < eastEdgeLim  = (outerMid, offset - innerMid)
  | offset < northEdgeLim = (- (offset - eastEdgeLim) + outerMid, outerMid)
  | offset < westEdgeLim  = (- outerMid, - (offset - northEdgeLim) + innerMid)
  | offset < southEdgeLim = ((offset - westEdgeLim) - outerMid, - outerMid)
  | otherwise = error "invariant failed. expected: 0 < offset < 4 * edge + 4"
  where eastEdgeLim  = edge
        northEdgeLim = eastEdgeLim + (1 + edge + 1)
        westEdgeLim  = northEdgeLim + edge
        southEdgeLim = westEdgeLim + (1 + edge + 1)
        innerMid = edge `div` 2
        outerMid = innerMid + 1

manhattanSpiralCoord :: Integer -> Coordinate
-- There's no 'inner' and 'outer' ring for (0, 0), so we special case it.
manhattanSpiralCoord 1 = (0, 0)
manhattanSpiralCoord index =
  -- The '-1' here is because the spiral is 1-indexed, not 0 indexed.
  let root = greatestOddSqrt (index - 1)
      spiralOffset = index - root * root - 1
   in offsetToCoord root spiralOffset

manhattanDist :: Coordinate -> Integer
manhattanDist (x, y) = abs x + abs y

manhattanSpiralDist :: Integer -> Integer
manhattanSpiralDist = manhattanDist . manhattanSpiralCoord

-- Sketch of part 2 algorithm:
--
-- 1. loop over [1..]
-- 2. spiral coord for it with manhattanSpiralCoord
-- 3. look up ALL 9 adjacent cells in neighborhood
--     --> i.e., you're keeping a map of Map.Map (Integer, Integer) Integer
-- 4. for the ones that return (Just foo)
--    --> sum their values
-- 5. write the answer into this coord

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]

type Board = Map.Map Coordinate Integer

spiralValueForIndex :: (Board, Integer) -> Integer -> (Board, Integer)
spiralValueForIndex (board, _) index =
  let coord = manhattanSpiralCoord index
   in case lookup' board coord of
     Just val -> (board, val)
     Nothing ->
       let val = sum . catMaybes . map (lookup' board) $ neighbors coord
           board' = Map.insert coord val board
        in (board', val)


underThreshold :: Ord a => a -> (t1, a) -> Bool
underThreshold threshold (_, value) = value <= threshold

firstSpiralGreater :: Integer -> Integer
firstSpiralGreater n =
  snd . head . dropWhile (underThreshold n) $ scanl spiralValueForIndex initial [1..]
    where initial = (Map.singleton (0, 0) 1, 0)

