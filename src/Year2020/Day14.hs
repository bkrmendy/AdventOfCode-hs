module Year2020.Day14 where

import Challenge
  
import qualified Data.Map.Strict as M
  
type Memory = M.Map Int Int
data MaskBit = X | I | O
type MaskState = [MaskBit]  
data Instruction
  = Mask [MaskBit]
  | Mem Int Int

instruction :: String -> Instruction
instruction = undefined

emptyMask :: [MaskBit]
emptyMask = replicate 36 X

run :: Memory -> MaskState -> [Instruction] -> Int
run mem _    []     = result mem
run mem mask (i:is) = undefined

result :: Memory -> Int
result = sum . M.elems

instance Challenge [Instruction] where
  parse = map instruction . lines
  partOne = show . run M.empty emptyMask 
  