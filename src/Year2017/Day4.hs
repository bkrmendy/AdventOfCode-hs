module Year2017.Day4 where
import Challenge

import Data.List (nubBy, sort)

type Criterium = String -> String -> Bool

valid :: Criterium -> [String] -> Bool
valid f str = length (nubBy f str) == length str

anagram :: Criterium
anagram s1 s2 = sort s1 == sort s2

solve :: Criterium -> [[String]] -> Int
solve f = length . filter (valid f)

instance Challenge [[String]] where
  parse = map words . lines
  partOne = show . solve (==)
  partTwo = show . solve anagram
