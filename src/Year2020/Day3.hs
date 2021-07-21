module Year2020.Day3 where

import Challenge
import Utils (mult)

import Prelude hiding (lookup)

import           Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A

type Grid = A.UArray (Int, Int) Char

tree :: Char -> Int
tree '#' = 1
tree _   = 0

lookup :: Grid -> (Int, Int) -> Maybe Char
lookup grid (c, r) = if r > h then Nothing else Just $ grid ! (c `rem` (w + 1), r)
  where (_, (w, h)) = A.bounds grid

slope :: (Int, Int) -> (Int, Int) -> Grid -> Int
slope (c, r) (dc, dr) grid =
  case grid `lookup` (c, r) of
    Nothing -> 0
    Just cc -> tree cc + slope (c + dc, r + dr) (dc, dr) grid

mkGrid :: [String] -> Grid
mkGrid strs = A.array ((0, 0), (w, h)) [ ((c, r), get c r) | c <- [0..w], r <- [0..h]]
  where
    (w, h) = (length (head strs) - 1, length strs - 1)
    get = \col row -> (strs !! row) !! col

instance Challenge Grid where
  parse = mkGrid . lines
  partOne = show . slope (0, 0) (3, 1)
  partTwo grid = show . mult $ map (\d -> slope (0, 0) d grid) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]