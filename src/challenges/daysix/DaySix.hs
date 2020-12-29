{-# LANGUAGE FlexibleInstances #-}

module DaySix where
import Challenge
import Utils (transpose)

import Data.List (sort, group, sortOn)
import Data.Ord

part1 :: [String] -> String
part1 = concatMap (take 1 . head . sortOn length . group . sort) . transpose

instance Challenge [String] where
  parse = lines
  partOne = part1
  partTwo = show

