module Year2017.Day23 where
import            Challenge
import            Utils (parseL, int)
import            Text.Parsec hiding (State)
import            Control.Monad.State
import qualified  Data.Map.Strict as M
import            Data.Numbers.Primes

data Operand = Immediate Int | Register Char
data Operation = Set | Sub | Mul | Jnz
data Instruction = Instruction Operation Operand Operand

data Machine = Machine { _regs :: M.Map Char Int, _ip :: Int, _program :: [Instruction] }

operand :: Parsec String () Operand
operand = (Immediate <$> int) <|> (Register <$> letter)

operation :: Parsec String () Operation
operation = try (Set <$ string "set") <|> (Sub <$ string "sub") <|> (Mul <$ string "mul") <|> (Jnz <$ string "jnz")

instruction :: Parsec String () Instruction
instruction = Instruction <$> operation <*> (space *> operand) <*> (space *> operand)

register :: Operand -> Char
register (Register r) = r
register _            = error "Not a register!"

value :: Operand -> State Machine Int
value (Immediate i) = return i
value (Register r) = gets $ M.findWithDefault 0 r . _regs

advance :: Int -> State Machine ()
advance offset = do
  ip <- gets _ip
  modify' $ \m -> m { _ip = ip + offset }

set :: Operand -> Operand -> State Machine ()
set x y = do
  valY <- value y
  modify' $ \m -> m { _regs = M.insert (register x) valY (_regs m) }
  advance 1

sub :: Operand -> Operand -> State Machine ()
sub x y = do
  valX <- value x
  valY <- value y
  modify' $ \m -> m { _regs = M.insert (register x) (valX - valY) (_regs m) }
  advance 1

mul :: Operand -> Operand -> State Machine ()
mul x y = do
  valX <- value x
  valY <- value y
  modify' $ \m -> m { _regs = M.insert (register x) (valX * valY) (_regs m) }
  advance 1

jnz :: Operand -> Operand -> State Machine ()
jnz x y = do
  valX <- value x
  valY <- value y
  if valX /= 0
    then advance valY
    else advance 1

interpret :: Instruction -> State Machine Operation
interpret (Instruction op x y) = do
  case op of
    Set -> set x y
    Sub -> sub x y
    Mul -> mul x y
    Jnz -> jnz x y
  return op

execute :: State Machine [Operation]
execute = do
  (Machine _ ip program) <- get
  if ip < length program
    then do
      op <- interpret (program !! ip)
      (:) op <$> execute
    else return []

pt1 :: [Instruction] -> Int
pt1 is = length [() | Mul <- evalState execute (Machine M.empty 0 is)]

-- | constants obtained by reverse engineering https://github.com/dp1/AoC17/blob/master/day23.5.txt
pt2 :: Int -> Int
pt2 base = length $ filter (not . isPrime) [b, b + 17 .. c]
  where
    b = base * 100 + 100000
    c = b + 17000


instance Challenge [Instruction] where
  parse     = parseL instruction
  partOne   = show . pt1
  partTwo   = show . pt2 . const 81