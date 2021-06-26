module Year2020.Day5 where

import Challenge

import Data.List (find)
import Data.Maybe (fromJust)

bin2Int :: [Int] -> Int
bin2Int = foldl (\a v -> a * 2 + v) 0

convert :: Char -> Int
convert 'F' = 0
convert 'L' = 0
convert 'B' = 1
convert 'R' = 1

seat :: String -> Int
seat = bin2Int . map convert

missing :: [Int] -> Int
missing is = fromJust $ find (`notElem` is) [lo..hi+1]
  where (lo, hi) = (minimum is, maximum is)

instance Challenge [String] where
  parse = lines
  partOne = show . maximum . map seat
  partTwo = show . missing . map seat


