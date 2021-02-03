{-# LANGUAGE FlexibleInstances #-}
module Year2018.Day16 where
import Challenge
import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Elfcode

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

allInstructions :: [Instruction]
allInstructions = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

threeOp :: Instruction -> Bool
threeOp Setr = False
threeOp Seti = False
threeOp _    = True

correct :: Opcode -> Registers -> Registers -> Instruction -> Bool
correct (Opcode codes) regBefore regAfter instr = exec (instr, tail codes) regBefore == regAfter

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
      doOp (Opcode (code:codes)) rs = exec ((mapping Map.! code), codes) rs
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