module Intcode (
  Program(Program, unProgram)
  , fromString
  , location
  , interpret
) where
import Utils
import Data.List.Split (splitOn)

newtype Program = Program { unProgram :: [Int] }

fromString :: String -> Program
fromString = Program . map (\i -> read i :: Int) . splitOn ","

location :: Int -> Program -> Int
location i (Program mem) = mem !! i

interpret :: Int -> Program -> Program
interpret pc (Program memory) = exec instruction
  where
    instruction = memory !! pc
    continue = interpret (pc + 4) . Program
    left = memory !! (pc + 1)
    right = memory !! (pc + 2)
    dst = memory !! (pc + 3)
    exec :: Int -> Program
    exec 99 = Program memory
    exec 1 = continue $ replace dst ((memory !! left) + (memory !! right)) memory
    exec 2 = continue $ replace dst ((memory !! left) * (memory !! right)) memory
    exec i = error ("Unsupported instruction: " ++ show i ++ " at position: " ++ show pc)
