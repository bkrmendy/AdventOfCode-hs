module Year2020.Day10 where

import Challenge
import Utils (readInt, count)

import Data.List (sort)

parseI :: String -> [Int]
parseI input = [0] ++ ns ++  [maximum ns + 3]
  where ns = (sort . map readInt . lines) input

-- | stolen from https://github.com/glguy/advent2020/blob/master/execs/Day10.hs, too good to leave there

pt1 :: [Int] -> Int
pt1 is = count 1 diffs * count 3 diffs
  where diffs = zipWith (-) (tail is) is


walk :: [Int] -> Integer -> Integer -> Integer -> Integer
walk (1:ds) x y z = walk ds y z (x + y + z)
walk (2:ds) _ y z = walk ds z 0 (y + z)
walk (3:ds) _ _ z = walk ds 0 0 z
walk []     _ _ z = z

pt2 :: [Int] -> Integer
pt2 is = walk diffs 0 0 1
  where diffs = zipWith (-) (tail is) is

instance Challenge [Int] where
  parse = parseI
  partOne = show . pt1
  partTwo = show . pt2

