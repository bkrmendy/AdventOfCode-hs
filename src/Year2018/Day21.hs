{-# LANGUAGE FlexibleInstances #-}
module Year2018.Day21 where
import Data.List.Split (splitOn)
import Challenge
import Elfcode

parseI :: String -> (Int, [(Instruction, [Int])])
parseI input = (read (drop 4 ip), ops)
  where
    (ip:instrs) = lines input
    fromLine [code, a, b, c] = (fromString code, [read a, read b, read c])
    ops = map (fromLine . splitOn " ") instrs

instance Challenge (Int, [(Instruction, [Int])]) where
  parse = parseI
  partOne = show . map show . snd
  partTwo = show . map show . snd

