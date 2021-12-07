module Year2021.Day7 where
  
import Challenge
import Utils (readInt)

import Data.List (sort)
import Data.List.Split (splitOn)

type Fuel = Int -> Int -> Int

tri :: Int -> Int
tri n = n * (n + 1) `div` 2

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

fuelPt1, fuelPt2 :: Fuel
fuelPt1 from to = abs (to - from)
fuelPt2 from to = tri $ abs (to - from)

alignTo :: Fuel -> Int -> [Int] -> Int
alignTo fuel pos = sum . map (fuel pos)

instance Challenge [Int] where
  parse = map readInt . splitOn ","
  partOne xs = show $ alignTo fuelPt1 (median xs) xs
  partTwo xs = show $ alignTo fuelPt2 (mean xs) xs
