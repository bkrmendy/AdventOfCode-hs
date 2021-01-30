module Intcode (
  Program(Program)
  , fromString
  , location
  , interpret
) where
import Utils
import Data.List.Split (splitOn)

digitAt :: Int -> Int -> Maybe Int
digitAt i n
  | z == 0 = Nothing
  | otherwise = Just $ (n `div` e) `mod` 10
  where
    e = 10 ^ i
    z = n `div` e

data ParameterMode = Immediate
                   | Position

fromMaybe :: Maybe Int -> ParameterMode
fromMaybe Nothing = Position
fromMaybe (Just 0) = Position
fromMaybe (Just 1) = Immediate
fromMaybe (Just n) = error ("Unrecognized parameter mode: " ++ show n)

data Instruction
  = Add ParameterMode ParameterMode ParameterMode
  | Mul ParameterMode ParameterMode ParameterMode
  | Write ParameterMode
  | Read ParameterMode
  | Hlt

newtype Program = Program [Int]

fromString :: String -> Program
fromString = Program . map (\i -> read i :: Int) . splitOn ","

location :: Int -> Program -> Int
location i (Program mem) = mem !! i

readInstruction :: Int -> Instruction
readInstruction i
  | opcode == 1 = Add leftMode rightMode dstMode
  | opcode == 2 = Mul leftMode rightMode dstMode
  | opcode == 3 = Read leftMode
  | opcode == 4 = Write leftMode
  | opcode == 99 = Hlt
  | otherwise = error ("Unrecongized instruction: " ++ show i)
  where
    opcode = i `mod` 100
    leftMode = fromMaybe (digitAt i 2)
    rightMode = fromMaybe (digitAt i 3)
    dstMode = fromMaybe (digitAt i 4)
    


interpret :: Int -> Program -> Program
interpret pc (Program memory) = exec instruction
  where
    instruction = memory !! pc
    continue = interpret (pc + 4) . Program
    l = memory !! (pc + 1)
    r = memory !! (pc + 2)
    d = memory !! (pc + 3)
    exec :: Int -> Program
    exec 99 = Program memory
    exec 1 = continue $ replace d ((memory !! l) + (memory !! r)) memory
    exec 2 = continue $ replace d ((memory !! l) * (memory !! r)) memory
    exec i = error ("Unsupported instruction: " ++ show i ++ " at position: " ++ show pc)
