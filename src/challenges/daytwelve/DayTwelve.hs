{-# LANGUAGE FlexibleInstances #-}

module DayTwelve where
import Challenge
import Data.Map
import Data.Function
import Text.Parsec as Parsec
import Utils

registers1 :: Map Char Int
registers1 = fromList [ ('a', 0) , ('b', 0) , ('c', 0) , ('d', 0) ]

registers2 :: Map Char Int
registers2 = fromList [ ('a', 0) , ('b', 0) , ('c', 1) , ('d', 0) ]

data OpCode
  = Register Char
  | Immediate Int

data Instruction
  = Inc Char
  | Dec Char
  | Jnz OpCode Int
  | Cpy OpCode Char

register = Register <$> letter
immediate = Immediate <$> int
opcode = register <|> immediate

inc = Inc <$> (string "inc " *> letter)
dec = Dec <$> (string "dec " *> letter)
jnz = Jnz <$> (string "jnz " *> opcode) <*> (string " " *> int)
cpy = Cpy <$> (string "cpy " *> opcode) <*> (string " " *> letter)

parseInstructions = Parsec.sepBy1 (cpy <|> inc <|> dec <|> jnz) Parsec.newline

run :: Map Char Int -> Int -> [Instruction] -> Map Char Int
run registers pc instructions
  | pc >= length instructions = registers
  | otherwise =
    case instructions !! pc of
        Inc reg ->
          unsafeGet reg registers
          & \val -> run (insert reg (val + 1) registers) (pc + 1) instructions
        Dec reg ->
          unsafeGet reg registers
          & \val -> run (insert reg (val - 1) registers) (pc + 1) instructions
        Jnz check offset ->
          (if valueOf check == 0 then 1 else offset)
          & \jump -> run registers (pc + jump) instructions
        Cpy src dest ->
          valueOf src
          & \val -> run (insert dest val registers) (pc + 1) instructions
    where
       valueOf (Register reg) = unsafeGet reg registers
       valueOf (Immediate imm) = imm

instance Challenge [Instruction] where
   parse = parseLines parseInstructions
   partOne = show . unsafeGet 'a' . run registers1 0
   partTwo = show . unsafeGet 'a' . run registers2 0
