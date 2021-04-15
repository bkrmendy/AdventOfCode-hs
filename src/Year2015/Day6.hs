module Year2015.Day6 where
import Challenge
import Utils (int, parseLines)

import Control.Monad.ST
import Control.Monad (forM_)
import qualified Data.Array.ST as Array
import Text.Parsec

type Coord = (Int, Int)
type Garden = Array.STUArray (Int, Int) Bool

data InstructionKind = Toggle | TurnOn | TurnOff
data Instruction = Instruction { _kind :: InstructionKind, _from :: Coord, _to :: Coord }

interpretBool :: InstructionKind -> Bool -> Bool
interpretBool Toggle  = not
interpretBool TurnOn  = const True
interpretBool TurnOff = const False

interpretInt :: InstructionKind -> Int -> Int
interpretInt Toggle x  = x + 2
interpretInt TurnOn x  = x + 1
interpretInt TurnOff x = max 0 (x - 1)

set :: Int -> Int -> Int
set = (+)

pCoord :: Parsec String () Coord
pCoord = (,) <$> int <*> (char ',' *> int)

pTurnOn :: Parsec String () Instruction
pTurnOn = Instruction TurnOn <$> (string "turn on " *> pCoord) <*> (string " through " *> pCoord)

pTurnOff :: Parsec String () Instruction
pTurnOff = Instruction TurnOff <$> (string "turn off " *> pCoord) <*> (string " through " *> pCoord)

pToggle :: Parsec String () Instruction
pToggle = Instruction Toggle <$> (string "toggle " *> pCoord) <*> (string " through " *> pCoord)

pInstruction :: Parsec String () Instruction
pInstruction = try pTurnOn <|> try pTurnOff <|> pToggle

execute :: [Instruction] -> Int
execute instructions = runST $ do
  garden <- Array.newArray ((0, 0), (999, 999)) False :: ST s (Array.STArray s (Int, Int) Bool)
  forM_ instructions $ \(Instruction i (fromR, fromC) (toR, toC)) -> do
    forM_ [(r, c) | r <- [fromR..toR], c <- [fromC..toC]] $ \c -> do
      val <- Array.readArray garden c
      Array.writeArray garden c (interpretBool i val)
  es <- Array.getElems garden
  return (length $ filter (== True) es)

execute2 :: [Instruction] -> Int
execute2 instructions = runST $ do
  garden <- Array.newArray ((0, 0), (999, 999)) 0 :: ST s (Array.STArray s (Int, Int) Int)
  forM_ instructions $ \(Instruction i (fromR, fromC) (toR, toC)) -> do
    forM_ [(r, c) | r <- [fromR..toR], c <- [fromC..toC]] $ \c -> do
      val <- Array.readArray garden c
      Array.writeArray garden c (interpretInt i val)
  es <- Array.getElems garden
  return $ sum es

instance Challenge [Instruction] where
  parse = parseLines (sepBy1 pInstruction newline)
  partOne = show . execute
  partTwo = show . execute2
  


