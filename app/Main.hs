module Main where

import Challenge
import Year2019.Day3

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2019/3.txt"
  let parsed = parse input :: [[Instr]]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
