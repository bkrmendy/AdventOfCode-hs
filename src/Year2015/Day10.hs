module Year2015.Day10 where
import Challenge
import Data.Char (digitToInt)
import Data.List (group)

step :: [Int] -> [Int]
step = concatMap (\g -> [length g, head g]) . group

process :: Int -> [Int] -> Int
process times = length . last . take times . iterate step

instance Challenge [Int] where
  parse = map digitToInt
  partOne = show . process 41
  partTwo = show . process 51

