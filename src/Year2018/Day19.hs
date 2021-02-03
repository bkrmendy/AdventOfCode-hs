{-# LANGUAGE FlexibleInstances #-}
module Year2018.Day19 where
import Data.List.Split (splitOn)
import Challenge
import Elfcode
import Utils (replace)

parseI :: String -> (Int, [(Instruction, [Int])])
parseI input = (read (drop 4 ip), ops)
  where
    (ip:instrs) = lines input
    fromLine [code, a, b, c] = (fromString code, [read a, read b, read c])
    ops = map (fromLine . splitOn " ") instrs



instance Challenge (Int, [(Instruction, [Int])]) where
  parse = parseI
  partOne = show . execute (Registers [0, 0, 0, 0, 0, 0])
  -- https://www.reddit.com/r/adventofcode/comments/a7j9zc/2018_day_19_solutions/ec3i62o
  partTwo = show . execute (Registers [1, 0, 0, 0, 0, 0])