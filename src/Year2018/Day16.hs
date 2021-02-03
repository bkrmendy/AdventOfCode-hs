{-# LANGUAGE FlexibleInstances #-}
module Year2018.Day16 where
import Challenge
import Utils (replace)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Bits
import qualified Data.Map as Map

newtype Registers = Registers { unRegisters :: [Int] } deriving (Eq, Show)
newtype Opcode = Opcode [Int] deriving Show
data Capture = Capture { before :: Registers, encoded :: Opcode, after :: Registers } deriving Show

toOpcode :: String -> Opcode
toOpcode = Opcode . map read . splitOn " "

fromString :: String -> Capture
fromString str = Capture (Registers $ read (drop bl b)) (toOpcode d) (Registers $ read (drop ba a))
  where
    (b:d:a:_) = splitOn "\n" str
    bl = length "Before: "
    ba = length "After:  "

parseI :: String -> ([Capture], [Opcode])
parseI input = (cs, os)
  where
    (captures:opcodes:_) = splitOn "\n\n\n\n" input
    cs = map fromString (splitOn "\n\n" captures)
    os = map toOpcode (splitOn "\n" opcodes)

data Instruction = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving (Eq, Show)

allInstructions :: [Instruction]
allInstructions = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

threeOp :: Instruction -> Bool
threeOp Setr = False
threeOp Seti = False
threeOp _    = True

gt :: Int -> Int -> Int
gt a b = if a > b then 1 else 0

eq :: Int -> Int -> Int
eq a b = if a == b then 1 else 0

exec :: Instruction -> Opcode -> Registers -> Registers
exec Addr (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) + (regs !! b)) regs)
exec Addi (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) + b          ) regs)

exec Mulr (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) * (regs !! b)) regs)
exec Muli (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) * b          ) regs)

exec Banr (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .&. (regs !! b)) regs)
exec Bani (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .&. b          ) regs)

exec Borr (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .|. (regs !! b)) regs)
exec Bori (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c ((regs !! a) .|. b          ) regs)

exec Setr (Opcode [_, a, _, c]) (Registers regs) = Registers (replace c (regs !! a) regs)
exec Seti (Opcode [_, a, _, c]) (Registers regs) = Registers (replace c a           regs)

exec Gtir (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c (gt a           (regs !! b)) regs)
exec Gtri (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c (gt (regs !! a) b)           regs)
exec Gtrr (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c (gt (regs !! a) (regs !! b)) regs)

exec Eqir (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c (eq a           (regs !! b)) regs)
exec Eqri (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c (eq (regs !! a)           b) regs)
exec Eqrr (Opcode [_, a, b, c]) (Registers regs) = Registers (replace c (eq (regs !! a) (regs !! b)) regs)

correct :: Opcode -> Registers -> Registers -> Instruction -> Bool
correct opcode regBefore regAfter instr = exec instr opcode regBefore == regAfter

correctFor :: [Capture] -> [([Instruction], Capture)]
correctFor = map (\c -> (correctForI c, c))
  where
    correctForI (Capture b o a) = filter (correct o b a) allInstructions

remove :: [([Instruction], Capture)] -> [(Int, Instruction)] -> [([Instruction], Capture)]
remove poss known = poss2
  where
    knownInstrs = [i | (_, i) <- known]
    poss2 = filter (not . null . fst) (map remI poss)
    remI (is, c) = ([i | i <- is, not (i `elem` knownInstrs)], c)

guessInstrs :: [(Int, Instruction)] -> [([Instruction], Capture)] -> [(Int, Instruction)]
guessInstrs acc [] = acc
guessInstrs acc possibilities = guessInstrs (acc ++ knownInstrs) (remove possibilities knownInstrs)
  where
     knownInstrs = [(code, head is) | (is, Capture _ (Opcode (code:_)) _) <- possibilities, length is == 1]

execProgram :: Map.Map Int Instruction -> Registers -> [Opcode] -> Registers
execProgram _ regs [] = regs
execProgram mapping regs (oc:rest) = execProgram mapping regs2 rest
  where
      doOp :: Opcode -> Registers -> Registers
      doOp oc@(Opcode (code:_)) regs = exec (mapping Map.! code) oc regs
      regs2 = doOp oc regs

disas ::  Map.Map Int Instruction -> [Opcode] -> [String]
disas _ [] = []
disas mapping (Opcode code:rest) = (show mnem ++ show (tail code)):disas mapping rest
  where mnem = (mapping Map.! (head code))

makeMapping :: [Capture] -> Map.Map Int Instruction
makeMapping = Map.fromList . nub . guessInstrs [] . correctFor

instance Challenge ([Capture], [Opcode]) where
  parse = parseI
  partOne = show . length . filter ((> 2) . length . fst) . correctFor . fst
  partTwo (cs, os) = show (execProgram (makeMapping cs) (Registers [0,0,0,0]) os)