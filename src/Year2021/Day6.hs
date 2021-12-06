module Year2021.Day6 where

import Utils (readInt, updateAtIndex)
  
import Data.List.Split (splitOn)
import Challenge

type School = [Int]

parseI :: String -> School
parseI ns = [ count n school | n <- [0..8]]
  where school = map readInt . splitOn "," $ ns
        count x = length . filter (== x)

step :: School -> School
step (s:schools) = nextSchools ++ [s]
  where nextSchools = updateAtIndex 6 (+ s) schools  
        
simulate :: Int -> School -> School
simulate 0 ns = ns
simulate n ns = simulate (n - 1) (step ns)

solve :: Int -> School -> Int
solve days = sum . simulate days 

instance Challenge School where
  parse = parseI
  partOne = show . solve 80
  partTwo = show . solve 256
