module Year2018.Day11 where
import Challenge
import Utils (sum2D)
import Data.List (sortOn, intercalate)

powerLevel :: Int -> Int -> Int -> Int
powerLevel gsn x y = final
  where
    rackID = x + 10
    powerLevelInitial = (rackID * y + gsn) * rackID
    hundreds = powerLevelInitial `div` 100 `mod` 10
    final = hundreds - 5

mkGrid :: Int -> [[Int]]
mkGrid gsn = [[ powerLevel gsn col row | col <- [1..300]] | row <- [1..300]]

summedAreaGrid :: [[Int]] -> [[Int]]
summedAreaGrid grid = [[_ | col <- [1..300] | row <- [1..300]]
  where
    

cutWindow :: (Int, Int)
          -> (Int, Int)
          -> [[Int]]
          -> [[Int]]
cutWindow (width, height) (col, row) grid = [take size $ drop col r | r <- rows]
  where rows = take size $ drop row grid

windows :: Int        -- ^ window size
        -> [[Int]]    -- ^ grid
        -> [Window]   -- ^ (X, Y, window)
windows _               [] = []
windows size grid
  | length grid < size || length (head grid) < size = []
  | otherwise = [(size, x + 1, y + 1, cutWindow (size, size) (x, y) grid) | x <- [1..maxCol], y <- [1..maxRow]]
    where
      maxCol = length (head grid) - size
      maxRow = length grid - size

type Window = (Int, Int, Int, [[Int]])

displayPartOne :: Window -> String
displayPartOne (_, x, y, _) = intercalate "," [show x, show y]

displayPartTwo :: Window -> String
displayPartTwo (s, x, y, _) = intercalate "," [show s, show x, show y]

windowSum :: Window -> Int
windowSum (_, _, _, w) = sum2D w

bestWindow :: [Window] -> Window
bestWindow = head . sortOn (negate . windowSum)

instance Challenge Int where
  parse i = read i :: Int
  partOne = displayPartOne . bestWindow . windows 3 . mkGrid
  partTwo = displayPartTwo . bestSizedWindow . mkGrid
