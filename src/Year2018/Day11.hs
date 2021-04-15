module Year2018.Day11 where
import Challenge
import Data.List (intercalate)
import Data.Array as A

powerLevel :: Int -> Int -> Int -> Int
powerLevel gsn x y = final
  where
    rackID = x + 10
    powerLevelInitial = (rackID * y + gsn) * rackID
    hundreds = powerLevelInitial `div` 100 `mod` 10
    final = hundreds - 5

type Grid = Array (Int, Int) Int

(!?) :: Ix i => Array i e -> e -> i -> e
(!?) a def idx = if A.inRange (bounds a) idx then a ! idx else def

mkGrid :: Int -> Grid
mkGrid gsn = array ((1, 1), (300, 300)) [ ((col, row), powerLevel gsn col row) | col <- [1..300], row <- [1..300]]

summedAreaGrid :: Grid -> Grid
summedAreaGrid grid = array ((1, 1), (300, 300)) [((col, row), value (col, row)) | col <- [1..300], row <- [1..300]]
  where value (c, r) = sum [grid ! (c', r') | c' <- [1..c], r' <- [1..r]]

type Window = ((Int, Int), Int, Int)

windows :: Int -> Grid -> [Window]
windows size grid = [((col, row), area (col, row) (col + size - 1, row + size - 1), size) | col <- [1..300-size]
                                                                                          , row <- [1..300-size]]
  where
    area (cMin, rMin) (cMax, rMax) = grid ! (cMax, rMax)
                                     + (!?) grid 0 (cMin - 1, rMin - 1)
                                     - (!?) grid 0 (cMin - 1, rMax)
                                     - (!?) grid 0 (cMax, rMin - 1)

windowsSmall :: Int -> Grid -> [Window]
windowsSmall size grid = [((col, row), area (col, row), size) | col <- [1..300-size+1], row <- [1..300-size+1]]
  where
    area (col, row) = sum [grid ! (c, r) | c <- [col..col + size - 1], r <- [row..row + size - 1]]

allWindows :: [Int] -> Grid -> [Window]
allWindows sz grid = map (maxWindow . flip windows grid) sz

displayPartOne :: Window -> String
displayPartOne ((x, y), _, _) = intercalate "," (map show [x, y])

displayPartTwo :: Window -> String
displayPartTwo ((x, y), _, size) = intercalate "," (map show [x, y, size])

maxWindow :: [Window] -> Window
maxWindow [] = ((0, 0), 0, 0)
maxWindow (a:as) = foldr go a as
  where
    go :: Window -> Window -> Window
    go p@(_, thisVal, _) n@(_, nextVal, _) = if nextVal > thisVal then n else p

instance Challenge Int where
  parse i = read i :: Int
  partOne = displayPartOne . maxWindow . windows 3 . summedAreaGrid . mkGrid
  partTwo = displayPartTwo . maxWindow . allWindows [1..300] . summedAreaGrid . mkGrid