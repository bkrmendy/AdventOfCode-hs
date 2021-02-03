module Elfcode (
  Registers(Registers)
  , Opcode(Opcode)
  , Instruction(Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr)
  , exec
  , fromString
  , execute
) where
import Utils (replace)
import Data.Bits ((.&.), (.|.))

newtype Registers = Registers { unRegisters :: [Int] } deriving (Eq, Show)
newtype Opcode = Opcode [Int] deriving Show
data Instruction = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving (Eq, Show)

fromString :: String -> Instruction
fromString "addi" = Addi
fromString "addr" = Addr

fromString "mulr" = Mulr
fromString "muli" = Muli

fromString "seti" = Seti
fromString "setr" = Setr

fromString "eqrr" = Eqrr
fromString "eqri" = Eqri

fromString "gtrr" = Gtrr
fromString "gtir" = Gtir
fromString "gtri" = Gtri

fromString "bani" = Bani
fromString "bori" = Bori

fromString m      = error ("Unrecognized mnemonic: " ++ show m)

gt :: Int -> Int -> Int
gt a b = if a > b then 1 else 0

eq :: Int -> Int -> Int
eq a b = if a == b then 1 else 0

exec :: (Instruction, [Int]) -> Registers -> Registers
exec (Setr, [a, _, c]) (Registers regs) = Registers (replace c (regs !! a) regs)
exec (Seti, [a, _, c]) (Registers regs) = Registers (replace c a           regs)

exec (Addr, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) + (regs !! b)) regs)
exec (Addi, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) + b          ) regs)

exec (Mulr, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) * (regs !! b)) regs)
exec (Muli, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) * b          ) regs)

exec (Gtir, [a, b, c]) (Registers regs) = Registers (replace c (gt a           (regs !! b)) regs)
exec (Gtri, [a, b, c]) (Registers regs) = Registers (replace c (gt (regs !! a) b)           regs)
exec (Gtrr, [a, b, c]) (Registers regs) = Registers (replace c (gt (regs !! a) (regs !! b)) regs)

exec (Eqir, [a, b, c]) (Registers regs) = Registers (replace c (eq a           (regs !! b)) regs)
exec (Eqri, [a, b, c]) (Registers regs) = Registers (replace c (eq (regs !! a)           b) regs)
exec (Eqrr, [a, b, c]) (Registers regs) = Registers (replace c (eq (regs !! a) (regs !! b)) regs)

exec (Banr, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .&. (regs !! b)) regs)
exec (Bani, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .&. b          ) regs)

exec (Borr, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .|. (regs !! b)) regs)
exec (Bori, [a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .|. b          ) regs)

executeTrace :: Registers -> (Int, [(Instruction, [Int])]) -> [Registers]
executeTrace (Registers regs) (ip, instrs)
  | pc >= length instrs = [Registers regs]
  | otherwise = Registers regsUpdated:executeTrace (Registers regsUpdated) (ip, instrs)
      where
        pc = regs !! ip
        (Registers regs2) = exec (instrs !! pc) (Registers regs)
        regsUpdated = replace ip ((regs2 !! ip) + 1) regs2
        
execute :: Registers -> (Int, [(Instruction, [Int])]) -> Registers
execute r i = last $ executeTrace r i