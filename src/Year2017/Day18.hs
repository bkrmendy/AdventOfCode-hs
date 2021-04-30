module Year2017.Day18 where
import            Challenge
import            Utils (int, parseL)

import            Text.Parsec hiding (State)
import qualified  Data.Map.Strict as M
import            Control.Monad.State.Lazy

data Machine = Machine { _regs :: M.Map Char Int
                       , _ip :: Int
                       , _running :: Bool
                       }

data ExecutionPt1 = ExPt1 { _machine :: Machine, _sounds :: [Int] }

data ExecutionPt2 = ExPt2 { _machine1 :: Machine, _sounds1 :: [Int]
                          , _machine2 :: Machine, _sounds2 :: [Int]
                          , _snd1 :: Int
                          }

data Effect = Output Int | Input Char

data Operand = Register Char | Immediate Int

data Instr
  = Snd Operand
  | Set Char Operand
  | Add Char Operand
  | Mul Char Operand
  | Mod Char Operand
  | Rcv Char
  | Jgz Operand Operand

pOperand :: Parsec String () Operand
pOperand = (Register <$> letter) <|> (Immediate <$> int)

pInstr :: Parsec String () Instr
pInstr = try (Snd <$> (string "snd " *> pOperand))
         <|> (Set <$> (string "set " *> letter) <*> (space *> pOperand))
         <|> (Add <$> (string "add " *> letter) <*> (space *> pOperand))
         <|> try (Mul <$> (string "mul " *> letter) <*> (space *> pOperand))
         <|> try (Mod <$> (string "mod " *> letter) <*> (space *> pOperand))
         <|> (Rcv <$> (string "rcv " *> letter))
         <|> (Jgz <$> (string "jgz " *> pOperand) <*> (space *> pOperand))

value :: Operand -> State Machine Int
value (Register  c) = do gets $ M.findWithDefault 0 c . _regs
value (Immediate v) = return v

sndI :: Operand -> State Machine Effect
sndI f = do
  Output <$> value f

set :: Char -> Operand -> State Machine ()
set x y = do
  val <- value y
  modify' $ \m -> m { _regs = M.insert x val (_regs m) }

op :: (Int -> Int -> Int) -> Char -> Operand -> State Machine ()
op f r o = do
    val <- value o
    reg <- gets $ M.findWithDefault 0 r . _regs
    modify' $ \m -> m { _regs = M.insert r (reg `f` val) (_regs m) }

add :: Char -> Operand -> State Machine ()
add = op (+)

mul :: Char -> Operand -> State Machine ()
mul = op (*)

modI :: Char -> Operand -> State Machine ()
modI = op mod

rcv :: Char -> State Machine (Maybe Effect)
rcv reg = do
  val <- gets $ M.findWithDefault 0 reg . _regs
  if val == 0
    then return Nothing
    else return $ Just (Input reg)
    
store :: Char -> Int -> State Machine ()
store reg v = do
  nextRegs <- gets $  M.insert reg v . _regs
  modify' $ \m -> m { _regs = nextRegs }

jgz :: Operand -> Operand -> State Machine ()
jgz x y = do
  offset <- value y
  condition <- value x
  ip <- gets _ip
  delta <- pure $ if condition > 0 then offset else 1
  modify' $ \m -> m { _ip = ip + delta }
  
advance :: State Machine ()
advance = modify' $ \m -> m { _ip = _ip m + 1 }

interpret :: Instr -> State Machine (Maybe Effect)
interpret (Snd o)   = advance   >> Just <$> sndI o
interpret (Set r o) = set r o   >> advance >> return Nothing
interpret (Add x y) = add x y   >> advance >> return Nothing
interpret (Mul x y) = mul x y   >> advance >> return Nothing
interpret (Mod x y) = modI x y  >> advance >> return Nothing
interpret (Rcv o)   = advance   >> rcv o
interpret (Jgz o c) = jgz o c              >> return Nothing

step :: [Instr] -> State Machine (Maybe Effect)
step is = do
  ip <- gets _ip
  if ip < length is
    then do
      i <- pure $ is !! ip
      interpret i
    else do
      modify' $ \m -> m { _running = False }
      return Nothing

pt1 :: [Instr] -> State ExecutionPt1 Int
pt1 is = do
  machine <- gets _machine
  (res, nextMachine) <- pure $ runState (step is) machine
  modify' $ \ex -> ex { _machine = nextMachine }
  case res of
    Nothing           -> pt1 is
    Just (Output o)   -> modify' (\ex -> ex { _sounds = _sounds ex ++ [o] }) >> pt1 is
    Just (Input _)    -> gets $ last . _sounds
  
processM1 :: Maybe Effect -> State ExecutionPt2 ()
processM1 res = do
  case res of
    Nothing           -> return ()
    Just (Output o)   -> modify' (\ex -> ex { _sounds2 = _sounds2 ex ++ [o] })
    Just (Input reg)  -> undefined

processM2 :: Maybe Effect -> State ExecutionPt2 ()
processM2 = undefined
    
pt2 :: [Instr] -> State ExecutionPt2 Int
pt2 is = do
  machine1 <- gets _machine1
  (res1, nextMachine1) <- pure $ runState (step is) machine1
  modify' $ \ex -> ex { _machine1 = nextMachine1 }      
  m1Running <- pure $ (_running nextMachine1)
  machine2 <- gets _machine2
  (res2, nextMachine2) <- pure $ runState (step is) machine2
  modify' $ \ex -> ex { _machine2 = nextMachine2 }      
  m2Running <- pure $ (_running nextMachine2)
  if not m1Running && not m2Running
    then gets _snd1
    else do
      processM1 res1
      processM2 res2
      pt2 is

instance Challenge [Instr] where
  parse = parseL pInstr
  partOne is = show $ evalState (pt1 is) (ExPt1 (Machine M.empty 0 True) [])