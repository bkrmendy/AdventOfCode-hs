module Day6 where
import Challenge
import Utils (transpose)
import Data.List (sort, group, sortOn)

solve :: (String -> Int) -> [String] -> String
solve compareFn = concatMap (take 1 . head . sortOn compareFn . group . sort) . transpose

instance Challenge [String] where
  parse = lines
  partOne = solve (negate . length)
  partTwo = solve length