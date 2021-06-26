{-# LANGUAGE MultiWayIf #-}
module Year2020.Day8 where

import Challenge
import Utils (readInt)

import qualified Data.Set as S
import Data.Vector ((//))
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Control.Applicative (empty)

data Opcode
  = Acc
  | Jmp
  | Nop
  deriving Show

data Instruction = Instruction { _code :: Opcode, _arg :: Int } deriving Show

type Program = V.Vector Instruction

data Machine = Machine { _program :: Program
                       , _pc :: Int
                       , _accumulator :: Int
                       , _journal :: S.Set Int
                       }

arg :: String -> Int
arg ('+':i) = readInt i
arg i       = readInt i

instruction :: String -> Instruction
instruction l = case words l of
  "nop":n:_ -> Instruction Nop (arg n)
  "acc":n:_ -> Instruction Acc (arg n)
  "jmp":n:_ -> Instruction Jmp (arg n)

advance :: Int -> State Machine ()
advance n = modify' $ \m -> m { _pc = _pc m + n }

execute :: Instruction -> State Machine ()
execute (Instruction Nop _) = advance 1
execute (Instruction Jmp n) = advance n
execute (Instruction Acc n) = do
  advance 1
  modify' $ \m -> m { _accumulator = _accumulator m + n}

data RunResult
  = InfiniteLoop Int
  | GoneAwry Int
  | Terminated Int
  deriving Show

step :: State Machine RunResult
step = do
  (Machine program pc acc journal) <- get
  if pc `S.member` journal
    then return (InfiniteLoop acc)
    else do
       modify' $ \m -> m { _journal = pc `S.insert` journal }
       if | pc == V.length program -> return (Terminated acc)
          | pc < 0 || pc > V.length program -> return (GoneAwry acc)
          | otherwise -> execute (program V.! pc) >> step


parseI :: String -> Program
parseI input = V.fromList $! instruction <$> lines input

runMachine :: Program -> RunResult
runMachine is = evalState step (Machine is 0 0 S.empty)

tweaks :: Program -> [Program]
tweaks p = do
  (line, i) <- zip [0..] (V.toList p)
  case i of
    (Instruction Nop n) -> pure $ p // [(line, Instruction Jmp n)]
    (Instruction Jmp n) -> pure $ p // [(line, Instruction Nop n)]
    _                   -> empty

pt2 :: Program -> Int
pt2 p = head $ do
  p' <- tweaks p
  case runMachine p' of
    Terminated res -> pure res
    _              -> empty

instance Challenge Program where
  parse = parseI
  partOne = show . runMachine
  partTwo = show . pt2

