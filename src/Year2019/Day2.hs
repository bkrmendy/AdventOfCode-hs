{-# LANGUAGE FlexibleInstances #-}
module Year2019.Day2 where
import Challenge
import Utils
import Data.List.Split (splitOn)
import qualified Intcode as IC

interpret :: Int -> [Int] -> [Int]
interpret pc memory = exec instruction
  where
    instruction = memory !! pc
    continue = interpret (pc + 4)
    left = memory !! (pc + 1)
    right = memory !! (pc + 2)
    dst = memory !! (pc + 3)
    exec :: Int -> [Int]
    exec 99 = memory
    exec 1 = continue $ replace dst ((memory !! left) + (memory !! right)) memory
    exec 2 = continue $ replace dst ((memory !! left) * (memory !! right)) memory
    exec i = error ("Unsupported instruction: " ++ show i ++ " at position: " ++ show pc)

doctor :: Int -> Int -> IC.Program -> IC.Program
doctor noun verb (IC.Program program)= IC.Program $ replace 1 noun $ replace 2 verb program

combo :: IC.Program -> [Int]
combo mem = [100 * n + v | n <- [0..100], v <- [0..100]
                         , IC.location 0 (IC.interpret 0 (doctor n v mem)) == 19690720]  

instance Challenge IC.Program where
  parse = doctor 12 2 . IC.fromString
  partOne = show . IC.location 0 . IC.interpret 0
  partTwo = show . head . combo 