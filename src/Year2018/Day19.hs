{-# LANGUAGE FlexibleInstances #-}
module Year2018.Day19 where
import Data.List.Split (splitOn)
import Challenge
import Elfcode
import Utils (replace)

fromString :: String -> Instruction
fromString "addi" = Addi
fromString "addr" = Addr

fromString "mulr" = Mulr
fromString "muli" = Muli

fromString "seti" = Seti
fromString "setr" = Setr

fromString "eqrr" = Eqrr
fromString "gtrr" = Gtrr

fromString m      = error ("Unrecognized mnemonic: " ++ show m)

parseI :: String -> (Int, [(Instruction, [Int])])
parseI input = (read (drop 4 ip), ops)
  where
    (ip:instrs) = lines input
    fromLine [code, a, b, c] = (fromString code, [read a, read b, read c])
    ops = map (fromLine . splitOn " ") instrs


execute :: Registers -> (Int, [(Instruction, [Int])]) -> Registers
execute (Registers regs) (ip, instrs)
  | pc >= length instrs = Registers regs
  | otherwise = execute (Registers regsUpdated) (ip, instrs)
    where
      pc = regs !! ip
      (Registers regs2) = exec (instrs !! pc) (Registers regs)
      regsUpdated = replace ip ((regs2 !! ip) + 1) regs2



instance Challenge (Int, [(Instruction, [Int])]) where
  parse = parseI
  partOne = show . execute (Registers [0, 0, 0, 0, 0, 0])
  -- https://www.reddit.com/r/adventofcode/comments/a7j9zc/2018_day_19_solutions/ec3i62o
  partTwo = show . execute (Registers [1, 0, 0, 0, 0, 0])