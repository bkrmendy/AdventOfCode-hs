module Year2020.Day1 where

import Challenge
import Utils (readInt)

pt1 :: (Eq a, Num a) => a -> [a] -> a
pt1 s is = head [a * b | a <- is, b <- is, a + b == s]

pt2 :: (Eq a, Num a) => a -> [a] -> a
pt2 s is = head [a * b * c | a <- is, b <- is, c <- is, a + b + c == s]

instance Challenge [Int] where
  parse = map readInt . lines
  partOne = show . pt1 2020
  partTwo = show . pt2 2020

