module Year2021.Day7 where
  
import Challenge
import Utils (readInt)

import Data.List.Split (splitOn)

type Fuel = Int -> Int -> Int

fuelPt1, fuelPt2 :: Fuel
fuelPt1 from to = abs (to - from)
fuelPt2 from to = sum $ map snd $ zip [a..b - 1] [1..]
  where (a, b) = (min from to, max from to)

align :: Fuel -> [Int] -> Int
align fuel positions = minimum $ map alignI [left .. right]
  where (left, right) = (minimum positions, maximum positions)
        alignI pos = sum $ map (fuel pos) positions

instance Challenge [Int] where
  parse = map readInt . splitOn ","
  partOne = show . align fuelPt1
  partTwo = show . align fuelPt2
