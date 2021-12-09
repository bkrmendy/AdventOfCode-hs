module Year2021.Day9 where
  
import            Challenge
import            Utils (runBFS)

import            Data.Char (digitToInt)
import            Data.List (sortOn)
import qualified  Data.Array.Unboxed as A
import            Control.Monad (guard)

type Seafloor = A.UArray (Int, Int) Int
type Point = (Int, Int)

parseI :: String -> Seafloor
parseI input = A.array ((0, 0), (rows, cols)) $ do
  (row, iRow) <- zip ls [0..]
  (tile, iCol) <- zip row [0..]
  pure ((iRow, iCol), digitToInt tile)
  where ls = lines input
        (rows, cols) = (length ls - 1, length (head ls) - 1)

neighbors :: Seafloor -> (Int, Int) -> [(Int, Int)]
neighbors m (row, col) = do
  ns <- [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
  guard $ A.inRange (A.bounds m) ns  
  pure ns

riskLevel :: Int -> Int
riskLevel = (+ 1)

isLowPoint :: Seafloor -> (Int, Int) -> Bool
isLowPoint m point = all ((p <) . (m A.!)) $ neighbors m point
  where p = m A.! point 

lowPoints :: Seafloor -> [(Int, Int)]
lowPoints m = [(r, c) | r <- [mir .. mar], c <- [mic .. mac], isLowPoint m (r, c)]
  where ((mir, mic), (mar, mac)) = A.bounds m
  
partOneI :: Seafloor -> Int
partOneI m = sum
            $ map (riskLevel . (m A.!))
            $ lowPoints m

basinSizeOf :: Seafloor -> (Int, Int) -> Int
basinSizeOf m start = length $ runBFS neighborsI [start] 
  where
    neighborsI p = do
      ns <- neighbors m p
      guard $ m A.! ns /= 9
      pure ns

partTwoI :: Seafloor -> Int
partTwoI m = product
            $ take 3
            $ sortOn negate
            $ map (basinSizeOf m) 
            $ lowPoints m 

instance Challenge Seafloor where
  parse = parseI
  partOne = show . partOneI
  partTwo = show . partTwoI