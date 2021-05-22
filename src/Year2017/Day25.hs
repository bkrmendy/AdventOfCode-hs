module Year2017.Day25 where
import Challenge
import Utils (int, parseLines)

import            Text.Parsec         hiding (State)
import            Prelude             hiding (Left, Right)
import qualified  Data.Map.Strict as M
import            Control.Monad.State hiding (state)

-- | Data
data Instruction = Write Int
                 | Left
                 | Right
                 | Continue Char
                 deriving Show

data InstructionBlock = InstructionBlock { _zero :: (Instruction, Instruction, Instruction)
                                         , _one  :: (Instruction, Instruction, Instruction)
                                         }
                                         deriving Show

data Program = Program { _steps :: Int
                       , _blocks :: M.Map Char InstructionBlock
                       }
                       deriving Show

data Tape = Tape { _tape :: M.Map Int Int
                 , _position :: Int
                 , _state :: Char
                 }

-- | Parsing
pState :: Parsec String () Char
pState = string "In state " *> letter <* char ':' <* newline

pZero :: Parsec String () ()
pZero = () <$ (string "  If the current value is 0:" *> newline)

pOne :: Parsec String () ()
pOne = () <$ (string "  If the current value is 1:" *> newline)

pWrite :: Parsec String () Instruction
pWrite = Write <$> (string "    - Write the value " *> int <* char '.' <* newline)

pLeft :: Parsec String () Instruction
pLeft = Left <$ (string "    - Move one slot to the left." *> newline)

pRight :: Parsec String () Instruction
pRight = Right <$ (string "    - Move one slot to the right." *> newline)

pContinue :: Parsec String () Instruction
pContinue = Continue <$> (string "    - Continue with state " *> letter <* char '.' <* newline)

pInstruction :: Parsec String () Instruction
pInstruction = try pWrite <|> try pLeft <|> try pRight <|> try pContinue

pBlock :: Parsec String () (Char, InstructionBlock)
pBlock = do
  state <- pState
  pZero
  z0 <- pInstruction
  z1 <- pInstruction
  z2 <- pInstruction
  pOne
  o0 <- pInstruction
  o1 <- pInstruction
  o2 <- pInstruction
  _ <- newline
  return (state, InstructionBlock (z0, z1, z2) (o0, o1, o2))

mkProgram :: Int -> String -> Program
mkProgram steps src = Program steps blocks
  where blocks = M.fromList $ parseLines (many1 pBlock) src

-- | Interpretation
executeInstruction :: Instruction -> State Tape ()
executeInstruction (Write i)    = modify' $ \(Tape tape pos state) -> Tape (M.insert pos i tape) pos state
executeInstruction Left         = modify' $ \(Tape tape pos state) -> Tape tape (pred pos) state
executeInstruction Right        = modify' $ \(Tape tape pos state) -> Tape tape (succ pos) state
executeInstruction (Continue c) = modify' $ \(Tape tape pos _) -> Tape tape pos c

executeInstructionBlock :: InstructionBlock -> State Tape ()
executeInstructionBlock (InstructionBlock zero one) = do
  cell <- gets $ \(Tape tape pos _) -> M.findWithDefault 0 pos tape
  let (i1, i2, i3) = if cell == 0 then zero else one
  executeInstruction i1
  executeInstruction i2
  executeInstruction i3

executeProgram :: Program -> State Tape ()
executeProgram (Program 0 _) = return ()
executeProgram (Program steps blocks) = do
  state <- gets _state
  executeInstructionBlock (blocks M.! state)
  executeProgram (Program (steps - 1) blocks)

ones :: State Tape Int
ones = gets $ M.size . M.filter (== 1) . _tape

-- | Taken out from input to make parsing easier:
-- ^ Begin in state A.
-- ^ Perform a diagnostic checksum after 12399302 steps.
instance Challenge Program where
  parse = mkProgram 12399302
  partOne program = show $ evalState (executeProgram program >> ones) (Tape M.empty 0 'A')
  partTwo _ = "Printer rebooted!"