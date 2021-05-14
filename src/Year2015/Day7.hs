module Year2015.Day7 where
import Challenge
import Utils (parseLines, int)

import Text.Parsec hiding (State)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Bits (complement, (.&.), (.|.), shiftL, shiftR)
import Data.Tuple (swap)
import Data.Word

-- | Data model
data Arg
  = Wire String
  | Const Word16
  deriving (Eq, Ord)

data BinaryOp = AND | OR | RSHIFT | LSHIFT
data UnaryOp  = NOT

data Op
  = Binary BinaryOp Arg Arg
  | Unary UnaryOp Arg
  | Constant Arg

binary :: Arg -> BinaryOp -> Arg -> Op
binary left op right = Binary op left right

data Node = Evaluated Word16 | Unevaluated Op

type Wires = Map.Map String Node

binOp :: BinaryOp -> Word16 -> Word16 -> Word16
binOp AND     a b = a .&. b
binOp OR      a b = a .|. b
binOp LSHIFT  a b = a `shiftL` fromIntegral b
binOp RSHIFT  a b = a `shiftR` fromIntegral b

-- | Evaluation

evaluateArg :: Arg -> State Wires Word16
evaluateArg (Const i) = return i
evaluateArg (Wire nd) = evaluate nd

evaluateUnary :: Arg -> State Wires Word16
evaluateUnary arg = complement <$> evaluateArg arg

evaluateBinary :: BinaryOp -> Arg -> Arg -> State Wires Word16
evaluateBinary op left right = do
  leftE <- evaluateArg left
  rightE <- evaluateArg right
  return $ binOp op leftE rightE

evaluateNode :: Node -> State Wires Word16
evaluateNode (Evaluated n) = return n
evaluateNode (Unevaluated (Unary NOT arg)) = evaluateUnary arg
evaluateNode (Unevaluated (Binary op left right)) = evaluateBinary op left right
evaluateNode (Unevaluated (Constant arg)) = evaluateArg arg

evaluate :: String -> State Wires Word16
evaluate wire = do
  wires <- get
  val <- evaluateNode (wires Map.! wire)
  modify $ Map.insert wire (Evaluated val) -- home made call-by-need
  return val

-- | Parsing

pWireId :: Parsec String () String
pWireId = many1 letter

pNum :: Parsec String () Word16
pNum = fromIntegral <$> int

pArg :: Parsec String () Arg
pArg = (Const <$> pNum) <|> (Wire <$> pWireId)

pBinOp :: Parsec String () BinaryOp
pBinOp = (AND <$ string "AND") <|> (OR <$ string "OR") <|> (LSHIFT <$ string "LSHIFT") <|> (RSHIFT <$ string "RSHIFT")

pUnary :: Parsec String () Op
pUnary = Unary <$> (NOT <$ string "NOT") <*> (space *> pArg)

pBinary :: Parsec String () Op
pBinary = binary <$> pArg <*> (space *> pBinOp) <*> (space *> pArg)

pConstant :: Parsec String () Op
pConstant = Constant <$> pArg

pOp :: Parsec String () Op
pOp = pUnary <|> try pBinary <|> pConstant

pWire :: Parsec String () (Node, String)
pWire = (,) <$> (Unevaluated <$> pOp) <*> (string " -> " *> pWireId)

tweak :: Wires -> Wires
tweak = Map.insert "b" (Unevaluated $ Constant $ Const 956)

instance Challenge Wires where
  parse = Map.fromList . map swap . parseLines (sepBy1 pWire newline)
  partOne = show . evalState (evaluate "a")
  partTwo = show . evalState (evaluate "a") . tweak
  