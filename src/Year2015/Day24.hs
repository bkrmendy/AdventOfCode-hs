{-# LANGUAGE FlexibleInstances #-}
module Year2015.Day24 where
import Challenge
import Data.List (sortOn)
import Utils (subsequencesOfSize)

solve :: Integer -> Int -> [Integer] -> Integer
solve weight sz = minimum . map product . sortOn length . filter ((==) weight . sum) . subsequencesOfSize sz . reverse

instance Challenge [Integer] where
  parse = map (\a -> read a :: Integer) . lines
  partOne = show . solve 520 6
  partTwo = show . solve 390 4
