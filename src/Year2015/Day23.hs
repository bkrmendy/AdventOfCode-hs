module Year2015.Day23 where
import Challenge
import Utils (int, parseLines)
import Text.Parsec hiding (State)
import Control.Monad.State

data Register = A | B deriving Show
data Opcode
  = Hlf Register
  | Tpl Register
  | Inc Register
  | Jmp Int
  | Jie Register Int
  | Jio Register Int
  deriving Show

data Machine = Machine {
    _a :: Int
  , _b :: Int
  , _iPtr :: Int
  , _program :: [Opcode]
}

setReg :: Register -> (Int -> Int) -> State Machine ()
setReg A f = modify $ \m -> m { _a = f (_a m) }
setReg B f = modify $ \m -> m { _b = f (_b m) }

getReg :: Register -> Machine -> Int
getReg A = _a
getReg B = _b

setIPtr :: (Int -> Int) -> State Machine ()
setIPtr f = modify $ \m -> m { _iPtr = f (_iPtr m) }

register :: Parsec String () Register
register = (A <$ char 'a') <|> (B <$ char 'b')

hlf :: Parsec String () Opcode
hlf = Hlf <$> (string "hlf " *> register)

tpl :: Parsec String () Opcode
tpl = Tpl <$> (string "tpl " *> register)

inc :: Parsec String () Opcode
inc = Inc <$> (string "inc " *> register)

jmp :: Parsec String () Opcode
jmp = Jmp <$> (string "jmp " *> int)

jie :: Parsec String () Opcode
jie = Jie <$> (string "jie " *> register <* string ", ") <*> int

jio :: Parsec String () Opcode
jio = Jio <$> (string "jio " *> register <* string ", ") <*> int

opcode :: Parsec String () Opcode
opcode = try jmp <|> try jie <|> try jio <|> hlf <|> tpl <|> inc

interpret :: Opcode -> State Machine ()
interpret (Hlf r) = do
  setReg r (`div` 2)
  setIPtr (+ 1)
interpret (Tpl r) = do
  setReg r (* 3)
  setIPtr (+ 1)
interpret (Inc r) = do
  setReg r (+ 1)
  setIPtr (+ 1)
interpret (Jmp offset) = setIPtr (+ offset)
interpret (Jio r offset) = do
  reg <- gets $ getReg r
  jump <- pure $ if reg == 1 then offset else 1
  setIPtr (+ jump)
interpret (Jie r offset) = do
  reg <- gets $ getReg r
  jump <- pure $ if even reg then offset else 1
  setIPtr (+ jump)

step :: State Machine ()
step = do
  (Machine _ _ i prog) <- get
  when (i < length prog) $ do
    interpret (prog !! i)
    step

instance Challenge [Opcode] where
  parse = parseLines (sepBy1 opcode newline)
  partOne prog = show . _b $ execState step (Machine 0 0 0 prog)
  partTwo prog = show . _b $ execState step (Machine 1 0 0 prog)
