module Main where

import Challenge
import Elfcode
import Year2018.Day19

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2018/19.txt"
  let parsed = parse input :: (Int, [(Instruction, [Int])])
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
