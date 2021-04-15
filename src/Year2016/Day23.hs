module Day23 where
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Challenge
import Data.Maybe (fromJust)

data Operand = Immediate Int | Register Char | Missing deriving (Show, Eq)
data Opcode = Cpy | Inc | Dec | Tgl | Jnz | Mul | Add deriving (Show)
data Instruction = Instruction { code :: Opcode, left :: Operand, right :: Operand } deriving (Show)

mkOpcode :: String -> Opcode
mkOpcode "cpy" = Cpy
mkOpcode "inc" = Inc
mkOpcode "dec" = Dec
mkOpcode "tgl" = Tgl
mkOpcode "jnz" = Jnz
mkOpcode "mul" = Mul
mkOpcode "add" = Add
mkOpcode opc   = error ("Opcode " ++ opc ++ " not recognized")

type Registers = Map.Map Char Int

mkInstruction :: String -> Instruction
mkInstruction line = parse' pieces
  where
    pieces = splitOn " " line

    parse' :: [String] -> Instruction
    parse' [opcode, _operand] = Instruction { code = mkOpcode opcode, left = operand _operand, right = Missing }
    parse' [opcode, _left, _right] = Instruction { code = mkOpcode opcode, left = operand _left, right = operand _right }
    parse' parts = error ("Too many operands: " ++ show parts)

    operand :: String -> Operand
    operand part
      | head part `elem` "abcd" = Register (head part)
      | otherwise = Immediate (read part :: Int)

inc :: Registers -> Char -> (Int -> Int) -> Registers
inc regs reg f = Map.insert reg (f $ fromJust $ Map.lookup reg regs) regs

set :: Registers -> Int -> Char -> Registers
set regs val dest = Map.insert dest val regs

tgl :: [Instruction] -> Int -> [Instruction]
tgl [] _ = []
-- one-argument
tgl ((Instruction Inc _left _right):rest) 0 = Instruction Dec _left _right:rest
tgl ((Instruction Dec _left _right):rest) 0 = Instruction Inc _left _right:rest
tgl ((Instruction Tgl _left _right):rest) 0 = Instruction Inc _left _right:rest
-- two-argument
tgl ((Instruction Jnz _left _right):rest) 0 = Instruction Cpy _left _right:rest
tgl ((Instruction Cpy _left _right):rest) 0 = Instruction Jnz _left _right:rest
tgl (hd:rest) n = hd : tgl rest (pred n)

run :: Int -> Registers -> [Instruction] -> Registers
run ip registers instructions
  | ip >= length instructions = registers
  | otherwise = step (instructions !! ip)
  where
    value (Immediate i) = i
    value (Register r) = fromJust $ Map.lookup r registers
    step (Instruction Tgl _operand _) = run (succ ip) registers (tgl instructions (value _operand + ip))
    step (Instruction Inc (Register r) Missing) = run (succ ip) (inc registers r succ) instructions
    step (Instruction Inc (Register r) v) = run (succ ip) (inc registers r (+ value v)) instructions
    step (Instruction Dec (Register r) _) = run (succ ip) (inc registers r pred) instructions
    step (Instruction Cpy _left (Register dest)) = run (succ ip) (set registers (value _left) dest) instructions
    step (Instruction Mul rl rr@(Register dest)) = run (succ ip) (set registers (value rl * value rr) dest) instructions
    step (Instruction Add rl rr@(Register dest)) = run (succ ip) (set registers (value rl + value rr) dest) instructions
    step (Instruction Jnz _x _y) = run ip' registers instructions
      where
        ip' = if value _x == 0 then succ ip else ip + value _y
    step _ = run (succ ip) registers instructions

registers1 :: Registers
registers1 = Map.fromList [('a', 7), ('b', 0), ('c', 0), ('d', 0)]

registers2 :: Registers
registers2 = Map.fromList [('a', 12), ('b', 0), ('c', 0), ('d', 0)]

instance Challenge [Instruction] where
  parse = map mkInstruction . lines
  partOne = show . Map.lookup 'a' . run 0 registers1
  partTwo = show . Map.lookup 'a' . run 0 registers2


